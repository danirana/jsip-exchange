open! Core
open! Async
open Jsip_types
open Jsip_gateway
module Harness = Jsip_test_harness.Harness

(* The disconnect policies log a warning via [Log.Global]; silence it here so
   the (timestamped, non-deterministic) log lines don't leak into expect
   output. We assert the disconnect via the pipe/registry state instead. *)
let () = Log.Global.set_output []

(* A trade report carrying [i] in its size, so a test can see which events
   survived a bounded market-data / audit pipe. Trade reports fan out to both
   market-data subscribers (for their symbol) and audit subscribers. *)
let trade i =
  Exchange_event.Trade_report
    { symbol = Harness.aapl
    ; price = Price.of_int_cents 10000
    ; size = Size.of_int i
    }
;;

let trade_size = function
  | Exchange_event.Trade_report { size; _ } -> Size.to_int size
  | ( Order_accept _ | Order_reject _ | Order_cancel _ | Fill _
    | Best_bid_offer_update _ | Cancel_reject _ ) as other ->
    raise_s [%message "expected a trade report" (other : Exchange_event.t)]
;;

(* An order-reject carrying [i] as its client-order-id, routed to
   [participant]'s session feed. *)
let reject_for participant i =
  Exchange_event.Order_reject
    { request =
        Harness.buy
          ~price_cents:15000
          ~participant
          ~client_order_id:(Client_order_id.of_int i)
          ()
    ; reason = "slow-consumer test"
    }
;;

let reject_cid = function
  | Exchange_event.Order_reject { request; _ } ->
    Client_order_id.to_int request.client_order_id
  | ( Order_accept _ | Order_cancel _ | Fill _ | Trade_report _
    | Best_bid_offer_update _ | Cancel_reject _ ) as other ->
    raise_s [%message "expected an order reject" (other : Exchange_event.t)]
;;

let%expect_test "market data: drop-oldest keeps the newest [budget] events" =
  let dispatcher = Dispatcher.create ~market_data_budget:3 () in
  let reader =
    Dispatcher.subscribe_market_data dispatcher [ Harness.aapl ]
  in
  (* Push ten events synchronously. The per-subscriber pump only runs at an
     async yield, so all ten pass through [push] first, and drop-oldest pins
     the buffer at the budget instead of growing to ten. *)
  List.iter (List.init 10 ~f:Fn.id) ~f:(fun i ->
    Dispatcher.dispatch dispatcher [ trade i ]);
  printf
    "buffered = %d (budget 3)\n"
    (Dispatcher.pipe_occupancy dispatcher).market_data_max;
  [%expect {| buffered = 3 (budget 3) |}];
  (* Let the pump run and read what survived: the newest three (sizes 7, 8,
     9). The older seven were dropped from the front. *)
  let%bind survived =
    match%map Pipe.read_exactly reader ~num_values:3 with
    | `Exactly q -> Queue.to_list q
    | `Fewer q -> Queue.to_list q
    | `Eof -> []
  in
  print_s [%sexp (List.map survived ~f:trade_size : int list)];
  [%expect {| (7 8 9) |}];
  return ()
;;

let%expect_test "session: disconnect at [budget], drop from registry" =
  let dispatcher = Dispatcher.create ~session_budget:3 () in
  let%bind () = Dispatcher.set_up_session dispatcher Harness.alice in
  let session =
    Hashtbl.find_exn (Dispatcher.sessions dispatcher) Harness.alice
  in
  let reader = Session.reader session in
  (* Five rejects for Alice, whose feed is never read. At budget 3, the
     fourth push disconnects her instead of buffering more. *)
  List.iter (List.init 5 ~f:Fn.id) ~f:(fun i ->
    Dispatcher.dispatch dispatcher [ reject_for Harness.alice i ]);
  printf "session closed = %b\n" (Session.is_closed session);
  [%expect {| session closed = true |}];
  (* The writer is closed, so [to_list] drains the buffered events and EOFs.
     The survivors are the FIRST three that fit before the cutoff (cids 0,
     1, 2) — unlike market data, a disconnect keeps the oldest and refuses
     the rest. *)
  let%bind drained = Pipe.to_list reader in
  print_s [%sexp (List.map drained ~f:reject_cid : int list)];
  [%expect {| (0 1 2) |}];
  (* The disconnect frees the participant from the registry, so a reconnect
     is not locked out. This runs on the pipe's close callback, so yield
     first. *)
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  printf
    "still registered = %b\n"
    (Hashtbl.mem (Dispatcher.sessions dispatcher) Harness.alice);
  [%expect {| still registered = false |}];
  return ()
;;

let%expect_test "audit: disconnect at [budget], drop from registry" =
  let dispatcher = Dispatcher.create ~audit_budget:3 () in
  let reader = Dispatcher.subscribe_audit dispatcher in
  printf
    "subscribers before = %d\n"
    (Dispatcher.For_testing.audit_subscriber_count dispatcher);
  [%expect {| subscribers before = 1 |}];
  (* Audit gets every event; at budget 3 the fourth push disconnects. *)
  List.iter (List.init 5 ~f:Fn.id) ~f:(fun i ->
    Dispatcher.dispatch dispatcher [ trade i ]);
  let%bind drained = Pipe.to_list reader in
  print_s [%sexp (List.map drained ~f:trade_size : int list)];
  [%expect {| (0 1 2) |}];
  (* Its bag cleanup runs on the close callback; yield, then it is gone. *)
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  printf
    "reader closed = %b, subscribers after = %d\n"
    (Pipe.is_closed reader)
    (Dispatcher.For_testing.audit_subscriber_count dispatcher);
  [%expect {| reader closed = true, subscribers after = 0 |}];
  return ()
;;
