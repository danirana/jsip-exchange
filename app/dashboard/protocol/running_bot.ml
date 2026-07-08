open! Core
open Jsip_types

type t =
  { participant : Participant.t
  ; kind : Bot_kind.t
  }
[@@deriving sexp, bin_io]
