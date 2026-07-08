(** The bots the dashboard can launch against the live exchange.

    One constructor per pathological bot in {!Jsip_bots}. Sent as the query
    of {!Jsip_dashboard_protocol.Rpcs.launch_bot_rpc}: the browser picks a
    kind in a dropdown, the dashboard server maps it to a bot spec and starts
    it. *)

open! Core

type t =
  | Noise_trader
  | Book_filler
  | Cancel_storm
  | Spammer
  | Slow_consumer
[@@deriving sexp, bin_io, compare, equal, enumerate]

(** Human-readable label for the launcher dropdown, e.g. ["noise trader"]. *)
val to_display_string : t -> string

(** A stable, opaque token used as the [<option>] value in the client and
    parsed back when the selection changes. *)
val to_value_string : t -> string

(** Parse a token produced by {!to_value_string}. Returns [None] on an
    unknown token rather than raising — raising is pathologically slow under
    js_of_ocaml, and the input comes straight from the DOM. *)
val of_value_string : string -> t option
