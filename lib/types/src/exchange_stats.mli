(** Infrastructure metrics for the exchange, streamed once per second over
    {!Jsip_gateway.Rpc_protocol.stats_rpc}.

    Unlike {!Exchange_event.t} (the audit log, which records what the market
    did), these describe the exchange *process* — its heap size and per-RPC
    latency. Keeping them on a separate stream keeps the audit log a clean
    record of market events. Each {!t} is one one-second snapshot; the
    consuming dashboard folds a rolling window of them into its panes. *)

open! Core

(** A bucketed distribution of latencies observed in one one-second interval.

    Bucketing (rather than raw samples) bounds a snapshot's size under load
    and lets the dashboard merge snapshots exactly — summing the per-bucket
    {!field-counts} across a window yields a histogram you can read a
    windowed p50/p90/p99 off of. *)
module Latency_histogram : sig
  (** Exclusive upper edge of each bucket, increasing. A sample lands in the
      first bucket it is below; samples at least the last edge land in a
      final overflow bucket. Shared by producer and consumer, so
      {!field-counts} carries no labels. *)
  val bucket_upper_bounds : Time_ns.Span.t list

  (** Number of buckets: [List.length bucket_upper_bounds + 1]. *)
  val bucket_count : int

  type t =
    { counts : int list (** one per bucket; [List.length = bucket_count] *)
    ; total : int (** [= List.sum counts] *)
    ; sum : Time_ns.Span.t (** total of all samples, for a mean *)
    }
  [@@deriving sexp, bin_io]

  (** All buckets empty. *)
  val empty : t

  (** The bucket a single latency falls in, an index into {!field-counts} in
      [[0, bucket_count - 1]]. See {!bucket_upper_bounds}. *)
  val bucket_index : Time_ns.Span.t -> int
end

(** One symbol's book depth: the touch ({!Bbo.t}) plus total resting size
    across all price levels on each side. *)
module Book_depth : sig
  type t =
    { symbol : Symbol.t
    ; bbo : Bbo.t
    ; resting_size_bid : Size.t
    ; resting_size_ask : Size.t
    }
  [@@deriving sexp, bin_io]
end

(** Worst-case queue depth of each family of outbound subscriber pipe, plus
    the slowest session's participant. A slow consumer surfaces here first. *)
module Pipe_occupancy : sig
  type t =
    { market_data_max : int
    ; audit_max : int
    ; session_max : int
    ; slowest_session : Participant.t option
    }
  [@@deriving sexp, bin_io]
end

(** Per-participant order rate (submits this interval) and current resting
    order count. *)
module Participant_activity : sig
  type t =
    { participant : Participant.t
    ; orders_last_interval : int
    ; resting_orders : int
    }
  [@@deriving sexp, bin_io]
end

(** Matching-loop backlog ([queue_depth]) and the gap between successive
    drain iterations ([max_gap]/[mean_gap]) over the interval. *)
module Engine_busyness : sig
  type t =
    { queue_depth : int
    ; max_gap : Time_ns.Span.t
    ; mean_gap : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io]
end

type t =
  { live_words : int
  (** [Gc.stat ().live_words]: words live on the OCaml heap *)
  ; heap_words : int (** words in the major heap *)
  ; top_heap_words : int (** high-water mark of [heap_words] *)
  ; submit_latency : Latency_histogram.t
  (** [submit_order_rpc] service time *)
  ; cancel_latency : Latency_histogram.t
  (** [cancel_order_rpc] service time *)
  ; book_depth : Book_depth.t list (** one entry per traded symbol *)
  ; pipe_occupancy : Pipe_occupancy.t
  ; participants : Participant_activity.t list
  ; engine : Engine_busyness.t
  }
[@@deriving sexp, bin_io]
