(** A dense integer identifier for a trading symbol.

    The exchange assigns each tradable instrument a small id: the [i]th
    symbol in the engine's fixed symbol set is id [i]. Unlike {!Symbol.t} (a
    human name like ["AAPL"]), a [Symbol_id.t] is what actually travels on
    the wire — it is compact and indexes the matching engine's book array
    directly.

    The type is [private int]: you can read the underlying int (via the [:>]
    coercion or {!to_int}) but not fabricate one implicitly — construction
    goes through {!of_int}. Because the id crosses the wire, sealing it
    against construction is not a security boundary (a peer can always
    deserialize one); validity is instead enforced where an id meets the book
    array, in the matching engine. See {!Jsip_order_book.Matching_engine}. *)

open! Core

type t = private int [@@deriving sexp, bin_io, compare, equal, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** [of_int i] is the id numbered [i]. Total: it does not range-check against
    any particular engine's symbol set — that check lives in the engine, the
    one place that knows how many symbols exist. *)
val of_int : int -> t

val to_int : t -> int

(** Renders the id as its integer (e.g. [to_string (of_int 7) = "7"]). This
    is deliberately name-free: in phase 1 the exchange speaks in ids end to
    end. *)
val to_string : t -> string
