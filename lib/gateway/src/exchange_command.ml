open! Core
open Jsip_types

(* first word of a command *)

module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving string ~case_insensitive]

  let is_buy_or_sell verb = match verb with Buy | Sell -> true | _ -> false

  let get_side verb =
    match verb with
    | Buy -> Side.Buy
    | Sell -> Side.Sell
    | _ -> failwith "Invalid side"
  ;;
end

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

(* let to_string = function | Buy -> "Buy" | Sell -> "Sell" | Book -> "Book"
   | Subscribe -> "Subscribe" *)

(* 1. Splits the line on spaces and takes the first word
   2. Parse it as a Verb.t. If it fails, return Error
   3. Match on the verb to parse the remaining arguments

   The default_participant optional argument replaces the purpose of
   Protocol.parse_command_with_default_participant: when present, it
   overrides the participant on parsed orders where no as <name> clause was
   given. If neither default_participant nor an as <name> clause is provided,
   fall back to "anonymous".

   When moving the order-parsing logic, also fix the time-in-force parsing:
   Protocol.parse_command hardcodes "IOC", "DAY", etc. as string literals,
   but Time_in_force already has a case-insensitive of_string derived from
   [@@deriving string]. Use it instead.

   Similarly, these abbreviations are hard-coded in error messages and usage
   strings, meaning this have to be manually updated every time the variant
   changes. Fortunately, [@@deriving enumerate] provides a val all : t list
   of the variant tags that you can use along with List.map and String.concat
   to add val all_str : string to Time_in_force. Use it in the error message
   for unrecognized values, so any new time-in-force variants will
   automatically appear.

   Apply the same principle to the usage string — use Time_in_force.all_str
   rather than writing "[DAY|IOC]". *)

let parse ?default_participant line =
  (* trims the input line and splits it by spaces *)
  let line = String.strip line in
  if String.is_empty line
  then Or_error.error_string "empty command"
  else (
    let parts =
      String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Or_error.error_string "empty command"
    (* extracts first word as the verb_str and matches it against predefined
       verbs *)
    | verb_str :: rest ->
      let open Or_error.Let_syntax in
      let capitalized_verb_str =
        String.capitalize (String.lowercase verb_str)
      in
      let%bind verb =
        try Or_error.return (Verb.of_string capitalized_verb_str) with
        | _ ->
          Or_error.errorf
            "unknown command: %s (expected BUY SELL BOOK or SUBSCRIBE)"
            verb_str
      in
      (* if verb is Buy or Sell, it expects symbol, size, price, and time in
         force *)
      (match rest with
       | symbol_str
         :: size_str
         :: price_str
         :: time_in_force_str
         :: rest_of_args
         when Verb.is_buy_or_sell verb ->
         let%bind size =
           match Int.of_string_opt size_str with
           | Some n when n > 0 -> Or_error.return n
           | Some _ -> Or_error.errorf "size must be positive"
           | None -> Or_error.errorf "invalid size: %s" size_str
         in
         let%bind price =
           try Or_error.return (Price.of_string price_str) with
           | exn ->
             Or_error.errorf
               "invalid price: %s\nexception: %s"
               price_str
               (Exn.to_string exn)
         in
         let%bind symbol =
           try Or_error.return (Symbol.of_string symbol_str) with
           | exn ->
             Or_error.errorf
               "invalid symbol: %s\nexception: %s"
               symbol_str
               (Exn.to_string exn)
         in
         let%bind time_in_force =
           try
             Or_error.return (Time_in_force.of_string time_in_force_str)
           with
           | _ ->
             Or_error.errorf
               "invalid time-in-force: %s\nexpected one of: %s"
               time_in_force_str
               Time_in_force.all_str
         in
         (* logic for participants *)
         let%bind participant =
           match rest_of_args with
           | [ "as"; name ] | [ "AS"; name ] ->
             Or_error.return (Participant.of_string name)
           | [] ->
             (match default_participant with
              | Some p -> Ok p
              | None -> Or_error.return (Participant.of_string "anonymous"))
           | _ -> Or_error.error_string "Invalid arguments"
         in
         let side = Verb.get_side verb in
         Or_error.return
           (Submit
              { symbol
              ; participant
              ; side
              ; price
              ; size = Size.of_int size
              ; time_in_force
              })
       (* if verb is Book or Subscribe, it expects symbol *)
       | symbol_str :: rest_of_args when not (Verb.is_buy_or_sell verb) ->
         let%bind symbol =
           try Or_error.return (Symbol.of_string symbol_str) with
           | exn ->
             Or_error.errorf
               "invalid symbol: %s\nexception: %s"
               symbol_str
               (Exn.to_string exn)
         in
         (match verb with
          | Book -> Or_error.return (Book symbol)
          | Subscribe -> Or_error.return (Subscribe symbol)
          | _ ->
            Or_error.errorf
              "unexpected arguments for command %s: %s"
              verb_str
              (String.concat ~sep:" " rest_of_args))
       | _ ->
         Or_error.errorf
           "unexpected arguments for command %s: %s"
           verb_str
           (String.concat ~sep:" " rest)))
;;
