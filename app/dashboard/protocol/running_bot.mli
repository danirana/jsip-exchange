(** One bot the dashboard has launched and is still tracking: its exchange
    identity plus which {!Bot_kind.t} it is. Returned by
    {!Jsip_dashboard_protocol.Rpcs.running_bots_rpc} so the client can list
    the bots it can stop. *)

open! Core
open Jsip_types

type t =
  { participant : Participant.t
  ; kind : Bot_kind.t
  }
[@@deriving sexp, bin_io]
