(** Server-global, additive map between participant names and their interned
    {!Participant_id.t}s. Shared across all connections through the single
    dispatcher: an id must mean the same participant to everyone who sees it
    (a fill names two participants), so there is exactly one registry.

    It is {b additive}: names are interned on login and never removed, so an
    id, once minted, stays valid for the whole run and a returning
    participant keeps the same id across reconnects. This is a different job
    — and a different lifetime — from the dispatcher's session table, which
    tracks who is currently connected and is pruned on disconnect. *)

open! Core
open Jsip_types

type t

val create : unit -> t

(** Return [name]'s stable id, minting a fresh one on first sight.
    Idempotent: interning the same name again returns the same id. *)
val intern : t -> Participant.t -> Participant_id.t

(** Resolve a name to its id for routing an event (events carry names).
    [None] only if the name has never been interned (never logged in). *)
val id_of_name : t -> Participant.t -> Participant_id.t option

(** Resolve an id back to its name at a name-speaking edge (events, display).
    Total: every id this registry minted has a name. *)
val name_of_id : t -> Participant_id.t -> Participant.t
