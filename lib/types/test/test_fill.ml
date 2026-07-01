open! Core
open Jsip_types

let%expect_test "notional_cents: price * size" =
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15025
     ; size = Size.of_int 100
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; aggressor_client_order_id = Client_order_id.of_int 1
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     ; resting_participant_client_order_id = Client_order_id.of_int 2
     }
     : Fill.t)
  in
  [%test_result: int] (Fill.notional_cents fill) ~expect:1502500
;;

let%expect_test "participant view " =
  let alice = Participant.of_string "Alice" in
  let bob = Participant.of_string "Bob" in
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15025
     ; size = Size.of_int 100
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; aggressor_client_order_id = Client_order_id.of_int 1
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     ; resting_participant_client_order_id = Client_order_id.of_int 2
     }
     : Fill.t)
  in
  let alice_view = Fill.to_participant_view fill alice in
  [%test_eq: string option]
    alice_view
    (Some "You bought 100 AAPL at $150.25");
  let bob_view = Fill.to_participant_view fill bob in
  [%test_eq: string option] bob_view (Some "You sold 100 AAPL at $150.25")
;;
