(** Bidirectional map between human symbol names ([Symbol.t]) and the integer
    ids that cross the wire ([Symbol_id.t]).

    The exchange speaks {!Jsip_types.Symbol_id} everywhere on the wire and
    inside the engine; this directory is the one place that also knows the
    names. It is built authoritatively in the server's [main] from the
    configured symbol set, served over {!Rpc_protocol.symbol_directory_rpc},
    and mirrored by each client and the monitor so they can show names while
    the wire still carries ids.

    Unlike the server-only {!Participant_registry}, this type is exported and
    replicated on every consumer. Unlike that registry it is fixed at
    construction — the symbol set does not grow — so there is no interning
    generator. *)

open! Core
open Jsip_types

type t

(** Build from explicit (name, id) pairs: the server's authoritative
    directory, and each consumer's mirror rebuilt from {!to_alist} over the
    RPC. Raises if a name or an id repeats. *)
val create : (Symbol.t * Symbol_id.t) list -> t

(** Build a nameless directory that labels each id by its own integer. For
    contexts that never had names (scenarios, e2e tests): {!name_of_id}
    returns the integer rendered as a [Symbol.t]. *)
val of_ids : Symbol_id.t list -> t

(** The id a typed name resolves to, or [None] if no such symbol is traded.
    Used at parse time to turn [BUY AAPL ...] into an id. *)
val id_of_name : t -> Symbol.t -> Symbol_id.t option

(** The name an id maps to, or [None] if this directory does not know the id
    (e.g. a mirror that is out of date). *)
val name_of_id : t -> Symbol_id.t -> Symbol.t option

(** A display label for an id: its name if known, else the integer rendered
    as a string. Total, so a stale mirror degrades to showing the id rather
    than failing at a render site. *)
val label : t -> Symbol_id.t -> string

(** The (name, id) pairs, for serving over the directory RPC. *)
val to_alist : t -> (Symbol.t * Symbol_id.t) list

(** Every id in the directory — i.e. the exchange's symbol set, which the
    server feeds to {!Jsip_order_book.Matching_engine.create}. *)
val ids : t -> Symbol_id.t list
