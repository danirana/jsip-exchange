(** The matching engine: receives order requests, manages order books, and
    produces exchange events.

    The engine is the heart of the exchange. It assigns order IDs, determines
    which orders can trade against each other, executes fills, and manages
    the lifecycle of resting orders. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** Create a matching engine for the given symbols. Each symbol gets its own
    order book. *)
val create : Symbol.t list -> t

(** {2 Order submission} *)

(** Submit a new order request. Returns the list of exchange events produced:
    an acceptance or rejection, followed by any fills, and possibly a
    cancellation of unfilled remainder (for IOC orders).

    The event list is always non-empty (at minimum an acceptance or
    rejection). *)
val submit : t -> Order.Request.t -> Exchange_event.t list

(** {2 Queries} *)

(** The order book for a given symbol, or [None] if the symbol is not traded
    on this engine. *)
val book : t -> Symbol.t -> Order_book.t option

(** How many orders each participant currently has resting across all books,
    aggregated from each book's incrementally-maintained count. *)
val resting_by_participant : t -> int Participant.Map.t

val cancel : t -> Participant.t -> Client_order_id.t -> Exchange_event.t list

(** Cancel every order the participant currently has resting, across all
    books — a mass-cancel / kill switch. Returns the [Order_cancel] events
    (plus any resulting BBO updates), or [[]] if they have nothing resting.
    Equivalent to calling {!cancel} on each of the participant's live orders. *)
val cancel_all_for_participant : t -> Participant.t -> Exchange_event.t list

(** Cancel every resting order on the exchange, across all participants — a
    whole-book reset. {!cancel_all_for_participant} folded over everyone;
    returns all the resulting [Order_cancel] (and BBO) events. *)
val cancel_everything : t -> Exchange_event.t list
