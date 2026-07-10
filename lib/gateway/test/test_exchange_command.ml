open! Core
open Jsip_types
open Jsip_gateway

(* Phase 2: a human types symbol NAMES again, and [parse] resolves them to
   ids through a directory. This test directory maps AAPL->0 and TSLA->1; the
   parsed request carries the id, so the echoed [Order.Request.to_string]
   still prints the id (its [to_string] is name-free) — the point of these
   tests is that typing a name resolves to the right id. *)
let test_directory =
  Symbol_directory.create
    [ Symbol.of_string "AAPL", Symbol_id.of_int 0
    ; Symbol.of_string "TSLA", Symbol_id.of_int 1
    ]
;;

let resolve_symbol = Symbol_directory.id_of_name test_directory

let print_parse ?default_participant line =
  match Exchange_command.parse ?default_participant ~resolve_symbol line with
  | Error err -> Printf.printf "ERROR: %s\n" (Error.to_string_hum err)
  | Ok action ->
    (match action with
     | Submit req -> Printf.printf "%s\n" (Order.Request.to_string req)
     | Book symbol -> Printf.printf "BOOK %s\n" (Symbol_id.to_string symbol)
     | Subscribe symbol ->
       Printf.printf "SUBSCRIBE %s\n" (Symbol_id.to_string symbol)
     | Cancel id ->
       Printf.printf "CANCEL %s\n" (Client_order_id.to_string id))
;;

let%expect_test "parse: basic buy" =
  print_parse "BUY 1 AAPL 100 150.25 DAY";
  [%expect {| BUY 1 0 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL 2 TSLA 50 200.00 DAY";
  [%expect {| SELL 2 1 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy 3 AAPL 100 150.00 DAY";
  print_parse "Buy 4 AAPL 100 150.00 DAY";
  [%expect
    {|
    BUY 3 0 100@$150.00 DAY as anonymous
    BUY 4 0 100@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY 5 AAPL 100 150.00 IOC";
  [%expect {| BUY 5 0 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY 6 AAPL 100 150.00 DAY as Alice";
  [%expect {| BUY 6 0 100@$150.00 DAY as Alice |}]
;;

(* --- Book & Subscribe Commands --- *)
let%expect_test "parse: book command" =
  print_parse "BOOK AAPL";
  [%expect {| BOOK 0 |}]
;;

let%expect_test "parse: subscribe command" =
  print_parse "SUBSCRIBE TSLA";
  [%expect {| SUBSCRIBE 1 |}]
;;

let%expect_test "parse: unknown symbol name is rejected" =
  print_parse "BUY 1 NOPE 100 150.00 DAY";
  print_parse "BOOK NOPE";
  [%expect
    {|
    ERROR: unknown symbol: NOPE
    ERROR: unknown symbol: NOPE
    |}]
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
  [%expect
    {| ERROR: unknown command: HOLD (expected BUY SELL BOOK SUBSCRIBE or CANCEL) |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY 7 AAPL abc 150.00 DAY";
  print_parse "BUY 8 AAPL 0 150.00 DAY";
  [%expect
    {|
    ERROR: invalid size: abc
    ERROR: size must be positive
    |}]
;;

(* --- default_participant testing --- *)
let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  print_parse ~default_participant:default "BUY 9 AAPL 100 150.00 DAY";
  [%expect {| BUY 9 0 100@$150.00 DAY as DefaultTrader |}]
;;

let%expect_test "default participant: overridden by explicit as" =
  let default = Participant.of_string "DefaultTrader" in
  print_parse
    ~default_participant:default
    "BUY 10 AAPL 100 150.00 DAY as Alice";
  [%expect {| BUY 10 0 100@$150.00 DAY as Alice |}]
;;

(* --- Book & Subscribe (case-insensitive) --- *)

let%expect_test "parse: subscribe with case-insensitive input" =
  print_parse "subscribe TSLA";
  print_parse "Subscribe TSLA";
  print_parse "SUBSCRIBE TSLA";
  [%expect {|
    SUBSCRIBE 1
    SUBSCRIBE 1
    SUBSCRIBE 1
    |}]
;;

(* --- Participant & override tests --- *)

let%expect_test "parse: default participant override" =
  let custom_parse =
    Exchange_command.parse
      ~default_participant:(Participant.of_string "MarketMaker")
      ~resolve_symbol
  in
  let print_custom line =
    match custom_parse line with
    | Error err -> Printf.printf "ERROR: %s\n" (Error.to_string_hum err)
    | Ok (Submit req) -> Printf.printf "%s\n" (Order.Request.to_string req)
    | Ok _ -> Printf.printf "unexpected non-submit action\n"
  in
  print_custom "BUY 11 AAPL 100 150.25 DAY";
  [%expect {| BUY 11 0 100@$150.25 DAY as MarketMaker |}]
;;

let%expect_test "parse: explicit as clause overrides default \
                 (case-insensitive)"
  =
  let custom_parse =
    Exchange_command.parse
      ~default_participant:(Participant.of_string "MarketMaker")
      ~resolve_symbol
  in
  let print_custom line =
    match custom_parse line with
    | Error err -> Printf.printf "ERROR: %s\n" (Error.to_string_hum err)
    | Ok (Submit req) -> Printf.printf "%s\n" (Order.Request.to_string req)
    | Ok _ -> Printf.printf "unexpected non-submit action\n"
  in
  print_custom "BUY 12 AAPL 100 150.25 DAY as Charlie";
  print_custom "BUY 13 AAPL 100 150.25 DAY AS Bob";
  [%expect
    {|
    BUY 12 0 100@$150.25 DAY as Charlie
    BUY 13 0 100@$150.25 DAY as Bob
    |}]
;;
