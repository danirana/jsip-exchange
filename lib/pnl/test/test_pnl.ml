open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness
open Harness

(* Hand-build a [Fill.t]. A fill always has two sides: the [aggressor]
   trading on [aggressor_side], and the [resting] participant taking the
   opposite side at the same price and size. The order/client ids are
   irrelevant to P&L, so we use fixed dummies. *)
let fill
  ~aggressor
  ~aggressor_side
  ~resting
  ?(symbol = aapl)
  ~price_cents
  ~size
  ()
  : Fill.t
  =
  { fill_id = 0
  ; symbol
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int 1
  ; aggressor_participant = aggressor
  ; aggressor_client_order_id = Client_order_id.of_int 1
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int 2
  ; resting_participant = resting
  ; resting_participant_client_order_id = Client_order_id.of_int 2
  }
;;

let print_summary pnl participant =
  print_endline (Participant.to_string participant ^ ":");
  print_endline (Pnl.Summary.to_string_hum (Pnl.summary pnl participant))
;;

(* Alice buys 100 AAPL @ $100 from Bob, then sells them back @ $110. Alice's
   realized gain is the mirror image of Bob's realized loss — P&L nets to
   zero across the two participants, which is a good sanity check. *)
let%expect_test "round trip nets between two participants" =
  let pnl =
    Pnl.empty
    |> fun t ->
    Pnl.apply_fill
      t
      (fill
         ~aggressor:alice
         ~aggressor_side:Buy
         ~resting:bob
         ~price_cents:10000
         ~size:100
         ())
  in
  let pnl =
    Pnl.apply_fill
      pnl
      (fill
         ~aggressor:alice
         ~aggressor_side:Sell
         ~resting:bob
         ~price_cents:11000
         ~size:100
         ())
  in
  print_summary pnl alice;
  print_summary pnl bob;
  [%expect
    {|
    Alice:
      AAPL: pos=0 avg=- ref=- realized=$1000.00 unrealized=$0.00
      total: realized=$1000.00 unrealized=$0.00 net=$1000.00
    Bob:
      AAPL: pos=0 avg=- ref=- realized=-$1000.00 unrealized=$0.00
      total: realized=-$1000.00 unrealized=$0.00 net=-$1000.00
    |}]
;;

(* Marking an open long against a trade print. Unrealized P&L moves with the
   reference price and is zero until a print is seen. *)
let%expect_test "unrealized marks against the last trade print" =
  let pnl =
    Pnl.apply_fill
      Pnl.empty
      (fill
         ~aggressor:alice
         ~aggressor_side:Buy
         ~resting:bob
         ~price_cents:10000
         ~size:100
         ())
  in
  (* No print yet: nothing to mark against. *)
  print_summary pnl alice;
  [%expect
    {|
    Alice:
      AAPL: pos=100 avg=$100.00 ref=- realized=$0.00 unrealized=$0.00
      total: realized=$0.00 unrealized=$0.00 net=$0.00
    |}];
  let pnl =
    Pnl.apply_trade_report
      pnl
      { symbol = aapl; price = Price.of_int_cents 10500 }
  in
  print_summary pnl alice;
  [%expect
    {|
    Alice:
      AAPL: pos=100 avg=$100.00 ref=$105.00 realized=$0.00 unrealized=$500.00
      total: realized=$0.00 unrealized=$500.00 net=$500.00
    |}];
  (* Print moves below cost: the unrealized mark goes negative. *)
  let pnl =
    Pnl.apply_trade_report
      pnl
      { symbol = aapl; price = Price.of_int_cents 9500 }
  in
  print_summary pnl alice;
  [%expect
    {|
    Alice:
      AAPL: pos=100 avg=$100.00 ref=$95.00 realized=$0.00 unrealized=-$500.00
      total: realized=$0.00 unrealized=-$500.00 net=-$500.00
    |}]
;;

(* Averaging up, then a sell that closes the whole position and flips to a
   short, then marking the short — plus a second symbol to exercise the
   per-symbol breakdown and totals. *)
let%expect_test "averaging, flipping, and multiple symbols" =
  let buy ?symbol ~price_cents ~size () =
    fill
      ~aggressor:alice
      ~aggressor_side:Buy
      ~resting:bob
      ?symbol
      ~price_cents
      ~size
      ()
  in
  let sell ?symbol ~price_cents ~size () =
    fill
      ~aggressor:alice
      ~aggressor_side:Sell
      ~resting:bob
      ?symbol
      ~price_cents
      ~size
      ()
  in
  let pnl =
    List.fold
      [ buy ~price_cents:10000 ~size:100 ()
      ; buy
          ~price_cents:10200
          ~size:100
          () (* AAPL avg -> $101.00 over 200 *)
      ; sell
          ~price_cents:10500
          ~size:250
          () (* close 200, flip to short 50 *)
      ; buy ~symbol:tsla ~price_cents:20000 ~size:50 ()
      ]
      ~init:Pnl.empty
      ~f:Pnl.apply_fill
  in
  let pnl =
    List.fold
      [ ({ symbol = aapl; price = Price.of_int_cents 10000 }
         : Pnl.Trade_report.t)
      ; { symbol = tsla; price = Price.of_int_cents 21000 }
      ]
      ~init:pnl
      ~f:Pnl.apply_trade_report
  in
  print_summary pnl alice;
  [%expect
    {|
    Alice:
      AAPL: pos=-50 avg=$105.00 ref=$100.00 realized=$800.00 unrealized=$250.00
      TSLA: pos=50 avg=$200.00 ref=$210.00 realized=$0.00 unrealized=$500.00
      total: realized=$800.00 unrealized=$750.00 net=$1550.00
    |}];
  (* The full structured summary, for the record. *)
  print_s [%sexp (Pnl.summary pnl alice : Pnl.Summary.t)];
  [%expect
    {|
    ((per_symbol
      (((symbol AAPL) (position -50) (average_entry_price (10500))
        (reference_price (10000)) (realized_cents 80000)
        (unrealized_cents 25000))
       ((symbol TSLA) (position 50) (average_entry_price (20000))
        (reference_price (21000)) (realized_cents 0) (unrealized_cents 50000))))
     (realized_cents 80000) (unrealized_cents 75000))
    |}]
;;
