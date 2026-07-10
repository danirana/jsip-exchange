(** The matching engine: receives order requests, manages order books, and
    produces exchange events.

    The engine is the heart of the exchange. It assigns order IDs, determines
    which orders can trade against each other, executes fills, and manages
    the lifecycle of resting orders. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** Create a matching engine trading the given symbol ids, one order book per
    id. The ids must be the dense range [0 .. n-1] (as
    {!Jsip_types.Symbol_id} values): only their count matters, since a book
    is reached by using the id directly as an array index. The engine speaks
    in {!Jsip_types.Symbol_id} everywhere below — no symbol names cross this
    boundary. *)
val create : Symbol_id.t list -> t

(** {2 Order submission} *)

(** Submit a new order request. Returns the list of exchange events produced:
    an acceptance or rejection, followed by any fills, and possibly a
    cancellation of unfilled remainder (for IOC orders).

    The event list is always non-empty (at minimum an acceptance or
    rejection). *)
val submit : t -> Order.Request.t -> Exchange_event.t list

(** {2 Queries} *)

(** The order book for a given symbol id, or [None] if the id is not a symbol
    traded on this engine (including an out-of-range id from an untrusted
    client). *)
val book : t -> Symbol_id.t -> Order_book.t option

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
