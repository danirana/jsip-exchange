open! Core
open Jsip_types
module Window = Jsip_dashboard_protocol.Window

(* The dashboard's pure state machine. Given the rolling window of per-second
   snapshots the server sends, it projects the exact numbers each pane draws:
   the memory time series, merged latency percentiles, and the latest book
   depth. No Bonsai here — [of_window] is plain data in, plain data out, so
   it is fully testable (see [test/test_controller.ml]). The Bonsai layer's
   whole job is to poll for a [Window.t], call [of_window], and render the
   result. *)

module Latency_summary = struct
  (* One bar of the latency histogram: how many samples fell in the bucket
     whose exclusive upper edge is [upper_bound] ([None] = the overflow
     bucket, i.e. slower than the largest edge). *)
  module Bucket = struct
    type t =
      { upper_bound : Time_ns.Span.t option
      ; count : int
      }
    [@@deriving sexp_of]
  end

  type t =
    { count : int (** total samples across the window *)
    ; p50 : Time_ns.Span.t option
    ; p90 : Time_ns.Span.t option
    ; p99 : Time_ns.Span.t option
    ; buckets : Bucket.t list (** merged histogram, for a bar chart *)
    }
  [@@deriving sexp_of]
end

module Display = struct
  type t =
    { memory_live_words : int list
    (** [live_words] per second, oldest to newest — the memory sparkline *)
    ; latest_live_words : int
    ; latest_heap_words : int
    ; latest_top_heap_words : int
    ; submit : Latency_summary.t
    ; cancel : Latency_summary.t
    ; book_depth : Exchange_stats.Book_depth.t list
    (** latest snapshot only *)
    ; pipe_occupancy : Exchange_stats.Pipe_occupancy.t option
    ; pipe_occupancy_series : int list
    (** worst pipe depth per second, oldest to newest — the pipe sparkline *)
    ; participants : Exchange_stats.Participant_activity.t list
    ; engine : Exchange_stats.Engine_busyness.t option
    ; engine_falling_behind : bool
    (** whether the matching engine is saturated, computed from the *latest*
        snapshot (per second) so the engine pane's alert tracks the live
        state rather than lagging behind a merged window *)
    ; sample_count : int (** how many seconds the window currently spans *)
    ; uptime_seconds : int
    (** total snapshots seen since connecting — an unbounded clock, unlike
        [sample_count] which caps at the window size *)
    }
  [@@deriving sexp_of]
end

let bucket_upper_bounds =
  Exchange_stats.Latency_histogram.bucket_upper_bounds
;;

let bucket_count = Exchange_stats.Latency_histogram.bucket_count

(* Sum the per-bucket counts across every snapshot in the window. All
   histograms share [bucket_upper_bounds], so this is an elementwise add. *)
let merge_counts (histograms : Exchange_stats.Latency_histogram.t list) =
  let zero = List.init bucket_count ~f:(fun _ -> 0) in
  List.fold histograms ~init:zero ~f:(fun acc { counts; _ } ->
    List.map2_exn acc counts ~f:( + ))
;;

(* The [quantile]th latency from merged bucket [counts]. We report the
   bucket's exclusive upper edge — a conservative "at most this slow" read —
   and [None] for the overflow bucket (slower than the largest edge) or an
   empty window. Cumulative-count walk: the percentile lands in the first
   bucket whose running total reaches the target rank. *)
let percentile ~counts ~total quantile =
  if total <= 0
  then None
  else (
    let target =
      Int.max 1 (Float.iround_up_exn (quantile *. Float.of_int total))
    in
    let rec walk index cumulative = function
      | [] -> None
      | count :: rest ->
        let cumulative = cumulative + count in
        if cumulative >= target
        then List.nth bucket_upper_bounds index
        else walk (index + 1) cumulative rest
    in
    walk 0 0 counts)
;;

let summarize (histograms : Exchange_stats.Latency_histogram.t list)
  : Latency_summary.t
  =
  let counts = merge_counts histograms in
  let total = List.sum (module Int) counts ~f:Fn.id in
  let buckets =
    List.mapi counts ~f:(fun index count ->
      { Latency_summary.Bucket.upper_bound =
          List.nth bucket_upper_bounds index
      ; count
      })
  in
  { count = total
  ; p50 = percentile ~counts ~total 0.50
  ; p90 = percentile ~counts ~total 0.90
  ; p99 = percentile ~counts ~total 0.99
  ; buckets
  }
;;

(* The matching-engine alert policy. Lives here (not in the Bonsai pane) so
   it is a single, unit-testable definition shared by the dashboard and the
   [verify_stats] observer. It fires when the engine can't keep up with the
   submit rate, via two ORed signals that fail in opposite directions:

   - [queue_depth] >= a few orders: a backlog caught in the act — direct, but
     an instantaneous sample the drain loop often empties between snapshots,
     so it reads 0 even under real saturation.

   - submit-latency saturation: covers exactly that blind spot. Submit
     latency is measured enqueue-to-handled, so it integrates the queueing
     the point sample misses. We trip on the MEDIAN (p50), not the tail: a
     healthy-but- loaded engine runs a spiky p99 of 10-100ms with its p50 at
     1ms, so a p99 rule false-fires; only genuine saturation drags the median
     past 100ms.

   We deliberately ignore the inter-iteration gap: it grows when the loop is
   merely idle between bursts as much as when overloaded. *)
let queue_backlog_threshold = 8
let saturation_p50_threshold = Time_ns.Span.of_ms 100.

let submit_saturated (submit : Latency_summary.t) =
  submit.count > 0
  &&
  match submit.p50 with
  (* [p50 = None] with samples present = median in the overflow bucket
     (slower than the largest edge, 1s) = saturation. With no samples it just
     means "no data", which [count > 0] already rules out. *)
  | None -> true
  | Some p50 -> Time_ns.Span.( > ) p50 saturation_p50_threshold
;;

let is_falling_behind
  ~(submit : Latency_summary.t)
  (engine : Exchange_stats.Engine_busyness.t)
  =
  engine.queue_depth >= queue_backlog_threshold || submit_saturated submit
;;

let of_window ({ samples; total_samples } : Window.t) : Display.t =
  let field f = List.map samples ~f in
  let latest = List.last samples in
  let latest_int f =
    match latest with Some (s : Exchange_stats.t) -> f s | None -> 0
  in
  { memory_live_words = field (fun (s : Exchange_stats.t) -> s.live_words)
  ; latest_live_words = latest_int (fun s -> s.live_words)
  ; latest_heap_words = latest_int (fun s -> s.heap_words)
  ; latest_top_heap_words = latest_int (fun s -> s.top_heap_words)
  ; submit =
      summarize (field (fun (s : Exchange_stats.t) -> s.submit_latency))
  ; cancel =
      summarize (field (fun (s : Exchange_stats.t) -> s.cancel_latency))
  ; book_depth =
      (match latest with
       | Some (s : Exchange_stats.t) -> s.book_depth
       | None -> [])
  ; pipe_occupancy =
      Option.map latest ~f:(fun (s : Exchange_stats.t) -> s.pipe_occupancy)
  ; pipe_occupancy_series =
      field (fun (s : Exchange_stats.t) ->
        let o = s.pipe_occupancy in
        Int.max (Int.max o.market_data_max o.audit_max) o.session_max)
  ; participants =
      (match latest with
       | Some (s : Exchange_stats.t) -> s.participants
       | None -> [])
  ; engine = Option.map latest ~f:(fun (s : Exchange_stats.t) -> s.engine)
  ; engine_falling_behind =
      (* Judge from the latest snapshot only, summarizing that one second's
         submit histogram — so the alert reflects the live engine, not a
         median smeared across the whole rolling window. *)
      (match latest with
       | None -> false
       | Some (s : Exchange_stats.t) ->
         is_falling_behind ~submit:(summarize [ s.submit_latency ]) s.engine)
  ; sample_count = List.length samples
  ; uptime_seconds = total_samples
  }
;;
