open! Core
open Jsip_types
open Jsip_gateway


let print_parse ?default_participant line =
  match Exchange_command.parse ?default_participant line with
  | Error err -> 
      (* Or_error.to_string_hum formats Core errors perfectly *)
      Printf.printf "ERROR: %s\n" (Error.to_string_hum err)
  | Ok action ->
      match action with
      | Submit req -> 
          (* Formats your Order.Request equivalent back to string *)
          Printf.printf "%s\n" (Order.Request.to_string req)
      | Book symbol -> 
          Printf.printf "BOOK %s\n" (Symbol.to_string symbol)
      | Subscribe symbol -> 
          Printf.printf "SUBSCRIBE %s\n" (Symbol.to_string symbol)
;;

let%expect_test "parse: basic buy" =
  print_parse "BUY AAPL 100 150.25 DAY";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL TSLA 50 200.00 DAY";
  [%expect {| SELL TSLA 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy AAPL 100 150.00 DAY";
  print_parse "Buy AAPL 100 150.00 DAY";
  [%expect {| 
    BUY AAPL 100@$150.00 DAY as anonymous 
    BUY AAPL 100@$150.00 DAY as anonymous 
  |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY AAPL 100 150.00 IOC";
  [%expect {| BUY AAPL 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY AAPL 100 150.00 DAY as Alice";
  [%expect {| BUY AAPL 100@$150.00 DAY as Alice |}]
;;

(* --- Book & Subscribe Commands --- *)
let%expect_test "parse: book command" =
  print_parse "BOOK AAPL";
  [%expect {| BOOK AAPL |}]
;;

let%expect_test "parse: subscribe command" =
  print_parse "SUBSCRIBE TSLA";
  [%expect {| SUBSCRIBE TSLA |}]
;;

(* --- Parse errors --- *)
let%expect_test "parse error: empty string" =
  print_parse "";
  print_parse "   ";
  [%expect {| 
    ERROR: empty command 
    ERROR: empty command 
  |}]
;;

let%expect_test "parse error: unknown command" =
  print_parse "HOLD AAPL 100 150.00 DAY";
  [%expect {| ERROR: unknown command: HOLD (expected BUY SELL BOOK or SUBSCRIBE) |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY AAPL abc 150.00 DAY";
  print_parse "BUY AAPL 0 150.00 DAY";
  [%expect {| 
    ERROR: invalid size: abc 
    ERROR: size must be positive 
  |}]
;;

(* --- default_participant testing --- *)
let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  print_parse ~default_participant:default "BUY AAPL 100 150.00 DAY";
  [%expect {| BUY AAPL 100@$150.00 DAY as DefaultTrader |}]
;;

let%expect_test "default participant: overridden by explicit as" =
  let default = Participant.of_string "DefaultTrader" in
  print_parse ~default_participant:default "BUY AAPL 100 150.00 DAY as Alice";
  [%expect {| BUY AAPL 100@$150.00 DAY as Alice |}]
;;

(* --- New Book & Subscribe Tests --- *)

let%expect_test "parse: book with symbol argument" =
  print_parse "BOOK AAPL" ;
  [%expect {| BOOK AAPL |}]
;;

let%expect_test "parse: subscribe with case-insensitive input" =
  print_parse "subscribe TSLA" ;
  print_parse "Subscribe TSLA" ;
  print_parse "SUBSCRIBE TSLA" ;
  [%expect {|
    SUBSCRIBE TSLA
    SUBSCRIBE TSLA
    SUBSCRIBE TSLA
  |}]
;;

(* --- New Participant & Overrides Tests --- *)

let%expect_test "parse: default participant override" =
  (* Tests passing a custom default participant via the optional argument *)
  let custom_parse = Exchange_command.parse ~default_participant:(Participant.of_string "MarketMaker") in
  let print_custom line =
    match custom_parse line with
    | Error err -> Printf.printf "ERROR: %s\n" (Error.to_string_hum err)
    | Ok action ->
      match action with
      | Submit req -> Printf.printf "%s\n" (Order.Request.to_string req)
      | Book symbol -> Printf.printf "BOOK %s\n" (Symbol.to_string symbol)
      | Subscribe symbol -> Printf.printf "SUBSCRIBE %s\n" (Symbol.to_string symbol)
  in
  print_custom "BUY AAPL 100 150.25 DAY" ;
  [%expect {| BUY AAPL 100@$150.25 DAY as MarketMaker |}]
;;

let%expect_test "parse: explicit as clause preservation" =
  (* Tests that an explicit 'as' clause overrides any default participant *)
  let custom_parse = Exchange_command.parse ~default_participant:(Participant.of_string "MarketMaker") in
  let print_custom line =
    match custom_parse line with
    | Error err -> Printf.printf "ERROR: %s\n" (Error.to_string_hum err)
    | Ok action ->
      match action with
      | Submit req -> Printf.printf "%s\n" (Order.Request.to_string req)
      | Book symbol -> Printf.printf "BOOK %s\n" (Symbol.to_string symbol)
      | Subscribe symbol -> Printf.printf "SUBSCRIBE %s\n" (Symbol.to_string symbol)
  in
  print_custom "BUY AAPL 100 150.25 DAY as Charlie" ;
  print_custom "BUY AAPL 100 150.25 DAY AS Bob" ;
  [%expect {|
    BUY AAPL 100@$150.25 DAY as Charlie
    BUY AAPL 100@$150.25 DAY as Bob
  |}]
;;
