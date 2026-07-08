(** Latency collection and stats fan-out for {!Rpc_protocol.stats_rpc}.

    Order-submit and order-cancel handlers call {!record_submit_latency} /
    {!record_cancel_latency} as they service requests. A once-per-second loop
    in {!Exchange_server} folds those tallies into an {!Exchange_stats.t} —
    via {!take_submit_histogram} / {!take_cancel_histogram} — and {!push}es
    it to every {!subscribe}r. This module owns the latency buckets and the
    subscriber set; it stays independent of the matching engine and [Gc],
    which the server's loop reads directly. *)

open! Core
open! Async
open Jsip_types

type t

(** A fresh collector with empty buckets and no subscribers. *)
val create : unit -> t

(** Record one [submit_order_rpc] service time (call to engine-handled). *)
val record_submit_latency : t -> Time_ns.Span.t -> unit

(** Record one [cancel_order_rpc] service time. *)
val record_cancel_latency : t -> Time_ns.Span.t -> unit

(** Snapshot the submit-latency buckets since the last call and clear them.
    Called once per second by the reporting loop. *)
val take_submit_histogram : t -> Exchange_stats.Latency_histogram.t

(** Snapshot and clear the cancel-latency buckets. See
    {!take_submit_histogram}. *)
val take_cancel_histogram : t -> Exchange_stats.Latency_histogram.t

(** Count one submitted order against its participant. *)
val record_order : t -> Participant.t -> unit

(** Snapshot and clear the per-participant submit counts. At a one-second
    cadence each count is that participant's orders/sec. *)
val take_order_counts : t -> (Participant.t * int) list

(** Record the elapsed time between two successive matching-loop iterations. *)
val record_engine_gap : t -> Time_ns.Span.t -> unit

(** Max and mean inter-iteration gap this interval, then reset. *)
val take_engine_gap : t -> Time_ns.Span.t * Time_ns.Span.t

(** A new reader that receives every future {!push}ed snapshot. The writer is
    dropped automatically when the reader is closed. *)
val subscribe : t -> Exchange_stats.t Pipe.Reader.t

(** Broadcast one snapshot to all current subscribers. Never blocks: a
    subscriber that has fallen behind simply misses writes past its buffer,
    same as the audit-log fan-out. *)
val push : t -> Exchange_stats.t -> unit
