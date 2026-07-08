open! Core

type t =
  | Noise_trader
  | Book_filler
  | Cancel_storm
  | Spammer
  | Slow_consumer
[@@deriving sexp, bin_io, compare, equal, enumerate]

let to_display_string = function
  | Noise_trader -> "noise trader"
  | Book_filler -> "book filler"
  | Cancel_storm -> "cancel storm"
  | Spammer -> "spammer"
  | Slow_consumer -> "slow consumer"
;;

(* A stable token for the [<option value>] round-trip. The sexp of a nullary
   constructor is just its name, which is exactly the opaque, URL-safe string
   we want. *)
let to_value_string t = Sexp.to_string (sexp_of_t t)

let of_value_string s =
  (* Never raise here: this parses a value straight out of the DOM, and
     raising under js_of_ocaml is pathologically slow. Look it up in [all]
     instead. *)
  List.find all ~f:(fun t -> String.equal (to_value_string t) s)
;;
