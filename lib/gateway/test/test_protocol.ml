open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

(* Phase 2: humans type symbol names; [parse] resolves them to ids through a
   directory. This one maps AAPL->0, TSLA->1, GOOG->2 (matching a default
   [Harness] engine). Rendering below still uses the default integer
   [render_symbol], so [format_event] output shows ids — the event-formatting
   tests are about the format, not the directory. *)
let test_directory =
  Symbol_directory.create
    [ Symbol.of_string "AAPL", Symbol_id.of_int 0
    ; Symbol.of_string "TSLA", Symbol_id.of_int 1
    ; Symbol.of_string "GOOG", Symbol_id.of_int 2
    ]
;;

let resolve_symbol = Symbol_directory.id_of_name test_directory

let print_parse ?default_participant line =
  match Exchange_command.parse ?default_participant ~resolve_symbol line with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok (Submit req) -> print_endline [%string "%{req#Order.Request}"]
  | Ok (Book sym) -> print_endline [%string "BOOK %{sym#Symbol_id}"]
  | Ok (Subscribe sym) ->
    print_endline [%string "SUBSCRIBE %{sym#Symbol_id}"]
  | Ok (Cancel id) -> print_endline [%string "CANCEL %{id#Client_order_id}"]
;;

(* --- Successful parsing: Buy & Sell --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY 101 AAPL 100 150.25 DAY";
  [%expect {| BUY 101 0 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL 102 TSLA 50 200.00 DAY";
  [%expect {| SELL 102 1 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy 103 AAPL 100 150.00 DAY";
  print_parse "Buy 104 AAPL 100 150.00 DAY";
  [%expect
    {|
    BUY 103 0 100@$150.00 DAY as anonymous
    BUY 104 0 100@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY 105 AAPL 100 150.00 IOC";
  [%expect {| BUY 105 0 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY 106 AAPL 100 150.00 DAY as Alice";
  [%expect {| BUY 106 0 100@$150.00 DAY as Alice |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse "     BUY 107  AAPL   100   150.00   DAY  ";
  [%expect {| BUY 107 0 100@$150.00 DAY as anonymous |}]
;;

(* --- Successful parsing: Book & Subscribe --- *)

let%expect_test "parse: book command" =
  print_parse "BOOK AAPL";
  [%expect {| BOOK 0 |}]
;;

let%expect_test "parse: subscribe command" =
  print_parse "SUBSCRIBE TSLA";
  [%expect {| SUBSCRIBE 1 |}]
;;

(* --- Parse errors --- *)

let%expect_test "parse error: empty string" =
  print_parse "";
  print_parse " ";
  [%expect {|
    ERROR: empty command
    ERROR: empty command
    |}]
;;

let%expect_test "parse error: unknown command" =
  print_parse " HOLD 108 AAPL 100 150.00 DAY";
  [%expect
    {| ERROR: unknown command: HOLD (expected BUY SELL BOOK SUBSCRIBE or CANCEL) |}]
;;

let%expect_test "parse error: unknown symbol name" =
  print_parse " BUY 108 NOPE 100 150.00 DAY";
  [%expect {| ERROR: unknown symbol: NOPE |}]
;;

let%expect_test "parse error: invalid client order ID" =
  print_parse " BUY abc AAPL 100 150.00 DAY";
  [%expect {| ERROR: invalid client order ID: abc |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse " BUY 109 AAPL abc 150.00 DAY";
  print_parse " BUY 110 AAPL 0 150.00 DAY";
  print_parse " BUY 111 AAPL -5 150.00 DAY";
  [%expect
    {|
    ERROR: invalid size: abc
    ERROR: size must be positive
    ERROR: size must be positive
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse " BUY 112 AAPL 100 xyz DAY";
  [%expect
    {|
    ERROR: invalid price: xyz
    exception: (Invalid_argument "Float.of_string xyz")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse " BUY 113 AAPL 100 150.00 QQQ";
  [%expect
    {|
    ERROR: invalid time-in-force: QQQ
    expected one of: DAY, IOC
    |}]
;;

(* --- default_participant optional parameter tests --- *)

let%expect_test "default participant: used when none specified" =
  let default_participant = Participant.of_string "DefaultTrader" in
  print_parse ~default_participant " BUY 114 AAPL 100 150.00 DAY";
  [%expect {| BUY 114 0 100@$150.00 DAY as DefaultTrader |}]
;;

let%expect_test "default participant: overridden by explicit 'as'" =
  let default_participant = Participant.of_string "DefaultTrader" in
  print_parse ~default_participant " BUY 115 AAPL 100 150.00 DAY as Alice";
  [%expect {| BUY 115 0 100@$150.00 DAY as Alice |}]
;;

(* --- Event formatting --- *)

let%expect_test "format_event: all event types" =
  let events =
    [ Exchange_event.Order_accept
        { order_id = Order_id.of_string "1"
        ; request =
            { symbol = Symbol_id.of_int 0
            ; participant = Participant.of_string "Alice"
            ; side = Buy
            ; price = Price.of_int_cents 15000
            ; size = Size.of_int 100
            ; time_in_force = Day
            ; client_order_id = Client_order_id.of_int 1
            }
        }
    ; Fill
        { fill_id = 1
        ; symbol = Symbol_id.of_int 0
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        ; aggressor_order_id = Order_id.of_string "2"
        ; aggressor_participant = Participant.of_string "Alice"
        ; aggressor_side = Buy
        ; aggressor_client_order_id = Client_order_id.of_int 1
        ; resting_order_id = Order_id.of_string "1"
        ; resting_participant = Participant.of_string "Bob"
        ; resting_client_order_id = Client_order_id.of_int 2
        }
    ; Order_cancel
        { order_id = Order_id.of_string "3"
        ; participant = Participant.of_string "Charlie"
        ; symbol = Symbol_id.of_int 1
        ; remaining_size = Size.of_int 50
        ; reason = Ioc_remainder
        ; client_order_id = Client_order_id.of_int 1
        }
    ; Order_reject
        { request =
            { symbol = Symbol_id.of_int 2
            ; participant = Participant.of_string "Alice"
            ; side = Sell
            ; price = Price.of_int_cents 28000
            ; size = Size.of_int 10
            ; time_in_force = Day
            ; client_order_id = Client_order_id.of_int 1
            }
        ; reason = "unknown symbol"
        }
    ; Best_bid_offer_update
        { symbol = Symbol_id.of_int 0
        ; bbo =
            { bid =
                Some
                  { price = Price.of_int_cents 14990
                  ; size = Size.of_int 200
                  }
            ; ask =
                Some
                  { price = Price.of_int_cents 15010
                  ; size = Size.of_int 100
                  }
            }
        }
    ; Best_bid_offer_update { symbol = Symbol_id.of_int 0; bbo = Bbo.empty }
    ; Trade_report
        { symbol = Symbol_id.of_int 0
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        }
    ]
  in
  List.iter events ~f:(fun e -> print_endline (Protocol.format_event e));
  [%expect
    {|
    ACCEPTED id=1 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=2)
    CANCELLED id=3 1 remaining=50 reason=IOC_REMAINDER cid=1
    REJECTED 2 SELL 10@$280.00 reason=unknown symbol
    BBO 0 bid=$149.90 x200 ask=$150.10 x100
    BBO 0 bid=- ask=-
    TRADE 0 $150.00 x100
    |}]
;;

(* --- Round-trip: parse then format --- *)

let%expect_test "round-trip: parse a command, submit, format result" =
  let open Jsip_test_harness in
  let t = Harness.create () in
  (* Place a resting sell *)
  Harness.submit_
    t
    (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  (* Parse a buy command from text and submit it *)
  let request =
    match
      Exchange_command.parse
        ~resolve_symbol
        "BUY 2 AAPL 100 150.00 DAY as Alice"
    with
    | Ok (Submit req) -> req
    | Ok _ -> failwith "Expected a Submit action"
    | Error err -> failwith (Error.to_string_hum err)
  in
  let events = Matching_engine.submit (Harness.engine t) request in
  print_endline (Protocol.format_events events);
  [%expect
    {|
    ACCEPTED id=1 0 SELL 100@$150.00 DAY
    BBO 0 bid=- ask=$150.00 x100
    ACCEPTED id=2 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x100 aggressor=2(Alice, cid=2) BUY resting=1(Bob, cid=1)
    TRADE 0 $150.00 x100
    BBO 0 bid=- ask=-
    |}]
;;
