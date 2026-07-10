(** A small, dense integer handle for a participant, minted at login and
    stable for the life of the process (so a participant keeps the same id
    across reconnects).

    Unlike {!Jsip_types.Participant.t} — the human name, which is the wire
    identity — a [Participant_id.t] never crosses the wire. It exists only to
    key the server's own tables cheaply. It therefore lives in the gateway,
    not beside the wire types, and is not re-exported from {!Jsip_gateway}.

    It is a [private int] so no code outside the minting path can fabricate
    one; ids come only from {!Generator} (used by {!Participant_registry}). *)

open! Core

type t = private int [@@deriving sexp_of, compare, equal, hash]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

(** Mints fresh, sequential ids. Encapsulated so ids can only be created here
    — mirroring {!Jsip_types.Order_id.Generator}. *)
module Generator : sig
  type participant_id := t
  type t [@@deriving sexp_of]

  val create : unit -> t

  (** Ids are handed out from [0] up, so an id doubles as a dense array index
      in {!Participant_registry}. *)
  val next : t -> participant_id
end

(** Integer conversions exposed only for tests. *)
module For_testing : sig
  val to_int : t -> int
  val of_int : int -> t
end
