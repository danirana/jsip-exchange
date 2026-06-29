open! Core
open Jsip_types

(* first word of a command *)

module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
    | Cancel
  [@@deriving string ~case_insensitive]

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
  | Cancel of Client_order_id.t

let parse ?default_participant line =
  (* trims the input line and splits it by spaces *)
  let line = String.strip line in
  if String.is_empty line then 
    Or_error.error_string "empty command"
  else (
    let parts = String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
    match parts with
    | [] -> Or_error.error_string "empty command"
    
    (* extracts first word as the verb_str and matches it against predefined verbs *)
    | verb_str :: rest -> 
      let open Or_error.Let_syntax in
      let capitalized_verb_str = String.capitalize (String.lowercase verb_str) in
      let%bind verb = 
        try Or_error.return (Verb.of_string capitalized_verb_str) with
        | _ -> Or_error.errorf "unknown command: %s (expected BUY SELL BOOK SUBSCRIBE or CANCEL)" verb_str 
      in
      
      match verb with
      (* Handle the text protocol CANCEL pattern command *)
      | Cancel -> (
          match rest with
          | [cl_id_str] -> (
              try Or_error.return (Cancel (Client_order_id.of_string cl_id_str)) with
              | _ -> Or_error.errorf "invalid client order ID: %s" cl_id_str
            )
          | _ -> Or_error.error_string "expected: CANCEL <client_order_id>"
        )

      (* if verb is Buy or Sell, it expects client id, symbol, size, price, and time in force *)

      | Buy | Sell -> (
          match rest with
          | client_order_id_str :: symbol_str :: size_str :: price_str :: time_in_force_str :: _ -> 
            let%bind client_order_id = 
              try Or_error.return (Client_order_id.of_string client_order_id_str) with
              | _ -> Or_error.errorf "invalid client order ID: %s" client_order_id_str 
            in 
            let%bind size = 
              match Int.of_string_opt size_str with
              | Some n when n > 0 -> Or_error.return n
              | Some _ -> Or_error.errorf "size must be positive"
              | None -> Or_error.errorf "invalid size: %s" size_str 
            in 
            let%bind price = 
              try Or_error.return (Price.of_string price_str) with
              | exn -> Or_error.errorf "invalid price: %s\nexception: %s" price_str (Exn.to_string exn) 
            in 
            let%bind symbol = 
              try Or_error.return (Symbol.of_string symbol_str) with
              | exn -> Or_error.errorf "invalid symbol: %s\nexception: %s" symbol_str (Exn.to_string exn) 
            in 
            let%bind time_in_force = 
              try Or_error.return (Time_in_force.of_string time_in_force_str) with
              | _ -> Or_error.errorf "invalid time-in-force: %s\nexpected one of: %s" time_in_force_str Time_in_force.all_str 
            in 
            let participant = 
                  match default_participant with
                  | Some p -> p
                  | None -> Participant.of_string "anonymous"
            in 
            let side = Verb.get_side verb in
            Or_error.return (Submit { symbol ; participant ; side ; price ; size = Size.of_int size ; time_in_force ; client_order_id })
          
          | _ -> 
            Or_error.errorf "expected: %s <client_order_id> <symbol> <size> <price> %s [as <name>]" 
              (String.uppercase verb_str) Time_in_force.all_str
        )

      (* if verb is Book or Subscribe, it expects symbol *)

      | Book | Subscribe -> (
          match rest with
          | symbol_str :: rest_of_args -> 
            if not (List.is_empty rest_of_args) then
              Or_error.errorf "unexpected arguments for command %s: %s" verb_str (String.concat ~sep:" " rest_of_args)
            else
              let%bind symbol = 
                try Or_error.return (Symbol.of_string symbol_str) with
                | exn -> Or_error.errorf "invalid symbol: %s\nexception: %s" symbol_str (Exn.to_string exn) 
              in 
              (match verb with
              | Book      -> Or_error.return (Book symbol)
              | Subscribe -> Or_error.return (Subscribe symbol)
              | _         -> assert false)
          | [] -> 
            Or_error.errorf "expected symbol for command %s" verb_str
        )
  )
;;