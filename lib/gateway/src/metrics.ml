open! Core
open! Async
open Jsip_types

(* Collects per-RPC latency into fixed buckets and fans one-second snapshots
   out to stats subscribers. Deliberately engine-agnostic: it knows nothing
   about order books or [Gc]. The server's reporting loop reads those, folds
   in {!take_submit_histogram}/{!take_cancel_histogram}, and hands a finished
   {!Exchange_stats.t} to {!push}. Subscribers attach and detach exactly like
   the audit-log subscribers in {!Dispatcher}. *)

(* One side's running tally for the current interval. Mutable and reset each
   second, so it holds at most one second of samples regardless of load. *)
module Accumulator = struct
  type t =
    { counts : int array
    ; mutable total : int
    ; mutable sum : Time_ns.Span.t
    }

  let create () =
    { counts =
        Array.create ~len:Exchange_stats.Latency_histogram.bucket_count 0
    ; total = 0
    ; sum = Time_ns.Span.zero
    }
  ;;

  let record t span =
    let index = Exchange_stats.Latency_histogram.bucket_index span in
    t.counts.(index) <- t.counts.(index) + 1;
    t.total <- t.total + 1;
    t.sum <- Time_ns.Span.( + ) t.sum span
  ;;

  (* Snapshot the interval as an immutable histogram, then clear for the next
     one. Draining on every tick — even with no subscribers — is what bounds
     memory. *)
  let take_and_reset t : Exchange_stats.Latency_histogram.t =
    let histogram =
      { Exchange_stats.Latency_histogram.counts = Array.to_list t.counts
      ; total = t.total
      ; sum = t.sum
      }
    in
    Array.fill t.counts ~pos:0 ~len:(Array.length t.counts) 0;
    t.total <- 0;
    t.sum <- Time_ns.Span.zero;
    histogram
  ;;
end

type t =
  { submit : Accumulator.t
  ; cancel : Accumulator.t
  ; subscribers : Exchange_stats.t Pipe.Writer.t Bag.t
  ; orders_by_participant : int Participant.Table.t
  ; mutable engine_gap_max : Time_ns.Span.t
  ; mutable engine_gap_sum : Time_ns.Span.t
  ; mutable engine_gap_count : int
  }

let create () =
  { submit = Accumulator.create ()
  ; cancel = Accumulator.create ()
  ; subscribers = Bag.create ()
  ; orders_by_participant = Participant.Table.create ()
  ; engine_gap_max = Time_ns.Span.zero
  ; engine_gap_sum = Time_ns.Span.zero
  ; engine_gap_count = 0
  }
;;

let record_submit_latency t span = Accumulator.record t.submit span
let record_cancel_latency t span = Accumulator.record t.cancel span
let take_submit_histogram t = Accumulator.take_and_reset t.submit
let take_cancel_histogram t = Accumulator.take_and_reset t.cancel

let record_order t participant =
  Hashtbl.incr t.orders_by_participant participant
;;

(* Snapshot the per-participant submit counts for this interval and clear for
   the next. At a one-second cadence each count is that participant's
   orders/sec. *)
let take_order_counts t =
  let counts = Hashtbl.to_alist t.orders_by_participant in
  Hashtbl.clear t.orders_by_participant;
  counts
;;

let record_engine_gap t span =
  t.engine_gap_max <- Time_ns.Span.max t.engine_gap_max span;
  t.engine_gap_sum <- Time_ns.Span.( + ) t.engine_gap_sum span;
  t.engine_gap_count <- t.engine_gap_count + 1
;;

(* Max and mean gap between drain-loop iterations this interval, then reset. *)
let take_engine_gap t =
  let max_gap = t.engine_gap_max in
  let mean_gap =
    if t.engine_gap_count = 0
    then Time_ns.Span.zero
    else
      Time_ns.Span.scale
        t.engine_gap_sum
        (1. /. Float.of_int t.engine_gap_count)
  in
  t.engine_gap_max <- Time_ns.Span.zero;
  t.engine_gap_sum <- Time_ns.Span.zero;
  t.engine_gap_count <- 0;
  max_gap, mean_gap
;;

let subscribe t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.subscribers elt);
  reader
;;

let push t snapshot =
  Bag.iter t.subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer snapshot)
;;
