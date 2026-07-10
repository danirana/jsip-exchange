open! Core

(* Infrastructure metrics for the exchange, streamed once per second over the
   dedicated stats RPC (see {!Jsip_gateway.Rpc_protocol.stats_rpc}). This is
   deliberately *not* an {!Exchange_event.t}: the audit log records domain
   events (accepts, fills, cancels), whereas these are operational metrics
   about the process itself — heap size and per-RPC latency. Keeping the two
   streams separate keeps the audit log a clean record of what the market
   did.

   Each value is one one-second snapshot. The consuming dashboard folds a
   rolling window (~60s) of these into the panes it draws, so the wire types
   here are chosen to be cheap to *merge* across snapshots. *)

module Latency_histogram = struct
  (* Latencies are bucketed rather than sent as raw samples so a snapshot's
     size is bounded no matter how many orders arrive in a second (a spammer
     bot can produce thousands). Buckets also merge exactly: to get a p99
     over the dashboard's rolling window you add the per-bucket [counts]
     across the window's snapshots and read the percentile off the summed
     histogram — something you cannot do with per-second precomputed
     percentiles.

     [bucket_upper_bounds] is the (exclusive) upper edge of each bucket, in
     increasing order; a sample lands in the first bucket whose bound it is
     below. Anything at least as large as the last bound lands in a final
     overflow bucket, so [counts] always has one more entry than there are
     bounds. Producer and consumer share this constant, so [counts] needs no
     labels on the wire. *)
  let bucket_upper_bounds =
    [ Time_ns.Span.of_us 1.
    ; Time_ns.Span.of_us 10.
    ; Time_ns.Span.of_us 100.
    ; Time_ns.Span.of_ms 1.
    ; Time_ns.Span.of_ms 10.
    ; Time_ns.Span.of_ms 100.
    ; Time_ns.Span.of_sec 1.
    ]
  ;;

  (* Number of buckets: one per bound, plus the overflow bucket. *)
  let bucket_count = List.length bucket_upper_bounds + 1

  type t =
    { counts : int list (** length is always {!bucket_count} *)
    ; total : int
    (** [= List.sum counts]; the number of samples this second *)
    ; sum : Time_ns.Span.t (** total of all samples, for a mean if wanted *)
    }
  [@@deriving sexp, bin_io]

  let empty =
    { counts = List.init bucket_count ~f:(fun _ -> 0)
    ; total = 0
    ; sum = Time_ns.Span.zero
    }
  ;;

  (* The bucket a single latency falls in: an index into [counts], between 0
     and [bucket_count - 1] inclusive. A sample lands in the first bucket
     whose exclusive upper edge it is strictly below; anything at least as
     large as the last edge falls through to the overflow bucket
     ([bucket_count - 1]). *)
  let bucket_index (span : Time_ns.Span.t) : int =
    match
      List.findi bucket_upper_bounds ~f:(fun _ bound ->
        Time_ns.Span.( < ) span bound)
    with
    | Some (index, _) -> index
    | None -> bucket_count - 1
  ;;
end

module Book_depth = struct
  (* The depth of one symbol's book at snapshot time: the touch (best bid and
     offer, via {!Bbo.t}) plus the *total* resting size across every price
     level on each side. The BBO tells you where the market is; the totals
     tell you how much interest is piled up behind it — which is what
     balloons when a book-filler bot rests orders it never means to trade. *)
  type t =
    { symbol : Symbol_id.t
    ; bbo : Bbo.t
    ; resting_size_bid : Size.t
    ; resting_size_ask : Size.t
    }
  [@@deriving sexp, bin_io]
end

module Pipe_occupancy = struct
  (* Current queue depth of the exchange's outbound subscriber pipes. A slow
     consumer shows up here as a growing queue while the others stay near
     zero. We report the worst (max) depth per category — the one that
     signals trouble — plus which session is worst, since a slow
     per-participant consumer is the usual culprit. *)
  type t =
    { market_data_max : int
    ; audit_max : int
    ; session_max : int
    ; slowest_session : Participant.t option
    }
  [@@deriving sexp, bin_io]
end

module Participant_activity = struct
  (* Per-participant footprint this interval: how many orders they submitted
     (→ orders/sec at a one-second cadence) and how many of their orders are
     currently resting on the book. *)
  type t =
    { participant : Participant.t
    ; orders_last_interval : int
    ; resting_orders : int
    }
  [@@deriving sexp, bin_io]
end

module Engine_busyness = struct
  (* How hard the matching loop is working. [queue_depth] is the request
     backlog waiting to be matched (sampled at snapshot time); [max_gap] and
     [mean_gap] are the elapsed time between successive iterations of the
     drain loop over the interval. Near-zero gaps with an empty queue =
     keeping up; growing gaps and a deep queue = falling behind. *)
  type t =
    { queue_depth : int
    ; max_gap : Time_ns.Span.t
    ; mean_gap : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io]
end

type t =
  { live_words : int
  (** [Gc.stat ().live_words]: words reachable on the OCaml heap right now —
      the headline memory number. One word is 8 bytes on 64-bit. *)
  ; heap_words : int (** total words allocated to the major heap *)
  ; top_heap_words : int (** high-water mark of [heap_words] *)
  ; submit_latency : Latency_histogram.t
  (** time from an order arriving on [submit_order_rpc] to the matching
      engine finishing it — includes time queued behind other work *)
  ; cancel_latency : Latency_histogram.t
  (** time to service one [cancel_order_rpc] call *)
  ; book_depth : Book_depth.t list (** one entry per traded symbol *)
  ; pipe_occupancy : Pipe_occupancy.t (** slow-consumer signal *)
  ; participants : Participant_activity.t list
  (** per-participant order rate + resting count *)
  ; engine : Engine_busyness.t (** matching-loop backlog and busyness *)
  }
[@@deriving sexp, bin_io]
