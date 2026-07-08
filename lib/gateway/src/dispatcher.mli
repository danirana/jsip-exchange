(** Central event-routing component for the gateway.

    Owns subscription registries:

    - **Market-data subscribers**, keyed by [Symbol.t]. Each subscriber gets
      a pipe of [Best_bid_offer_update] and [Trade_report] events for the
      symbol they asked about. This is the public market-data feed.

    - **Audit subscribers**, an unfiltered firehose of every event the
      matching engine produces. Intended for the exchange operator's monitor;
      not appropriate to expose to ordinary clients.

    [dispatch] is the single place that decides "for each event, who gets
    it". *)

open! Core
open! Async
open Jsip_types

type t

val clean_up_session : t -> Session.t -> unit Deferred.t
val set_up_session : t -> Participant.t -> unit Deferred.t
val sessions : t -> Session.t Participant.Table.t
val push_to_session : t -> Participant.t -> Exchange_event.t -> unit

(** Build a {!Session.t} carrying this dispatcher's configured session
    budget, so every session shares one slow-consumer disconnect threshold.
    The caller registers it (in {!sessions} and its connection state). *)
val create_session : t -> Participant.t -> Session.t

(** Create a dispatcher with empty subscription registries.

    Events whose audience is a single participant (order-lifecycle responses
    and [Fill] events) are routed to that participant's {!Session} outbound
    pipe by [push_to_session].

    {2 Slow-consumer policy}

    Every outbound pipe is bounded so one slow reader can't grow the
    exchange's memory without limit. The policy differs by family, because
    the value of the data does:

    - {b Market data} is a state stream — the newest BBO supersedes the last
      — so a full buffer {b drops the oldest} event. The slow subscriber
      keeps the freshest quotes with a gap in history. Bounded by
      [market_data_budget].

    - {b Session} and {b audit} are event streams whose records are not
      superseded (a missed [Fill] is a silently wrong position). A subscriber
      that falls [session_budget] / [audit_budget] behind is instead
      {b disconnected} — its pipe is closed and it must reconnect and resync,
      which is honest about the failure rather than losing events unnoticed.

    Each budget defaults to a sensible constant; pass explicit values to
    tune. *)
val create
  :  ?market_data_budget:int
  -> ?session_budget:int
  -> ?audit_budget:int
  -> unit
  -> t

(** Subscribe to public market data for one or more [symbols]. The same pipe
    receives events for every requested symbol; the dispatcher avoids
    duplicates so a subscriber listed against multiple symbols only sees each
    event once. The pipe is removed from the dispatcher when its reader is
    closed. *)
val subscribe_market_data
  :  t
  -> Symbol.t list
  -> Exchange_event.t Pipe.Reader.t

(** Subscribe to the full unfiltered event firehose. Intended for the monitor
    / admin tools. *)
val subscribe_audit : t -> Exchange_event.t Pipe.Reader.t

(** Route each event to every interested subscriber:

    - Every event is pushed to every audit subscriber.
    - [Best_bid_offer_update] and [Trade_report] are pushed to the
      market-data subscribers that asked for the event's symbol.
    - [Order_accept], [Order_cancel], and [Order_reject] are pushed to the
      session of the order's owning participant (if logged in).
    - [Fill] is pushed to both the aggressor's and the resting party's
      session (if either is logged in).

    Each session lookup is O(1) and independent of subscriber count. *)
val dispatch : t -> Exchange_event.t list -> unit

(** Worst current queue depth across each family of outbound subscriber pipe
    (per-symbol market data, audit, per-session), plus which session is
    furthest behind. A slow consumer shows up here as a growing queue. *)
val pipe_occupancy : t -> Exchange_stats.Pipe_occupancy.t

module For_testing : sig
  val audit_subscriber_count : t -> int
end
