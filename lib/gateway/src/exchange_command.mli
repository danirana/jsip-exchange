open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
  | Cancel of Client_order_id.t

(** Parse a command line. [resolve_symbol] turns a typed symbol name into its
    wire id — the client supplies {!Symbol_directory.id_of_name} over the
    directory it fetched at connect — so [BUY AAPL ...] resolves [AAPL] to an
    id and an unknown name fails with ["unknown symbol: ..."]. *)
val parse
  :  ?default_participant:Participant.t
  -> resolve_symbol:(Symbol.t -> Symbol_id.t option)
  -> string
  -> t Or_error.t
