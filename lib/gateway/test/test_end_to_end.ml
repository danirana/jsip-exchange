(** End-to-end tests with a real server and RPC clients.

    These tests spin up an actual exchange server on a local port, connect
    one or more clients via RPC, log them in, and verify the full path:
    client -> network -> server -> matching engine -> dispatcher -> session
    feed -> client. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_test_harness
open E2e_helpers

(* ---------------------------------------------------------------- *)
(* Multiple client tests *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: two clients trade with each other" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    (* Bob places a sell *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell ~price_cents:15000 ~participant:Harness.bob ())
    in
    [%expect {| [Bob] ACCEPTED id=1 AAPL SELL 100@$150.00 DAY |}];
    (* Alice places a buy — should cross *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [Alice] ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
      [Alice] FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [Bob] FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=1)
      |}];
    return ())
;;

let%expect_test "e2e: three clients, sequential orders, shared book" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind charlie = connect_as ~port Harness.charlie in
    (* Bob posts a sell *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell
           ~price_cents:15000
           ~size:50
           ~participant:Harness.bob
           ())
    in
    [%expect {| [Bob] ACCEPTED id=1 AAPL SELL 50@$150.00 DAY |}];
    (* Charlie posts a sell at a higher price *)
    let%bind () =
      rpc_submit
        charlie
        (Harness.sell
           ~price_cents:15010
           ~size:50
           ~participant:Harness.charlie
           ())
    in
    [%expect {| [Charlie] ACCEPTED id=2 AAPL SELL 50@$150.10 DAY |}];
    (* Alice buys 80 — should sweep through both *)
    let%bind () =
      rpc_submit alice (Harness.buy ~price_cents:15010 ~size:80 ())
    in
    [%expect
      {|
      [Alice] ACCEPTED id=3 AAPL BUY 80@$150.10 DAY
      [Alice] FILL fill_id=1 AAPL $150.00 x50 aggressor=3(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [Alice] FILL fill_id=2 AAPL $150.10 x30 aggressor=3(Alice, cid=1) BUY resting=2(Charlie, cid=1)
      [Bob] FILL fill_id=1 AAPL $150.00 x50 aggressor=3(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [Charlie] FILL fill_id=2 AAPL $150.10 x30 aggressor=3(Alice, cid=1) BUY resting=2(Charlie, cid=1)
      |}];
    (* Verify book state *)
    let%bind book = rpc_book alice Harness.aapl in
    print_endline (Option.value_exn book |> Book.to_string);
    [%expect
      {|
      === AAPL ===
        BIDS: (empty)
        ASKS:
          $150.10 x20
        BBO: - / $150.10 x20
      |}];
    return ())
;;

(* ---------------------------------------------------------------- *)
(* Market data subscription tests *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: market data subscriber receives trade and BBO updates" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind sub = connect_as ~port (Participant.of_string "Sub") in
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind result =
      Rpc.Pipe_rpc.dispatch
        Rpc_protocol.market_data_rpc
        (connection sub)
        [ Harness.aapl ]
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[MD Subscriber] %{e}"]));
    (* Post a sell *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell ~price_cents:15000 ~participant:Harness.bob ())
    in
    [%expect
      {|
      [Bob] ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
      [MD Subscriber] BBO AAPL bid=- ask=$150.00 x100
      |}];
    (* Cross it with a buy *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [Alice] ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
      [Alice] FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [Bob] FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [MD Subscriber] TRADE AAPL $150.00 x100
      [MD Subscriber] BBO AAPL bid=- ask=-
      |}];
    return ())
;;

let%expect_test "e2e: subscriber only sees events for subscribed symbol" =
  with_server ~symbols:[ Harness.aapl; Harness.tsla ] (fun ~server:_ ~port ->
    let%bind sub = connect_as ~port (Participant.of_string "Sub") in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind result =
      Rpc.Pipe_rpc.dispatch
        Rpc_protocol.market_data_rpc
        (connection sub)
        [ Harness.aapl ]
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[MD Subscriber] %{e}"]));
    (* Post on TSLA — subscriber should NOT see this *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell
           ~price_cents:20000
           ~symbol:Harness.tsla
           ~participant:Harness.bob
           ())
    in
    [%expect {| [Bob] ACCEPTED id=1 TSLA SELL 100@$200.00 DAY |}];
    (* Post on AAPL — subscriber SHOULD see this *)
    let base_request =
      Harness.sell ~price_cents:15000 ~participant:Harness.bob ()
    in
    let request_with_unique_id =
      { base_request with client_order_id = Client_order_id.of_int 2 }
    in
    let%bind () = rpc_submit bob request_with_unique_id in
    [%expect
      {| 
      [Bob] ACCEPTED id=2 AAPL SELL 100@$150.00 DAY 
      [MD Subscriber] BBO AAPL bid=- ask=$150.00 x100 
    |}];
    return ())
;;

(* ---------------------------------------------------------------- *)
(* Concurrent submission test *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: many clients submit orders concurrently" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind seed = connect_as ~port Harness.bob in
    let%bind () =
      Deferred.List.iter
        (List.init 10 ~f:Fn.id)
        ~how:`Sequential
        ~f:(fun i ->
          let base_request =
            Harness.sell ~price_cents:(15000 + i) ~participant:Harness.bob ()
          in
          let request_with_id =
            { base_request with
              client_order_id = Client_order_id.of_int (i + 1)
            }
          in
          rpc_submit seed request_with_id |> Deferred.ignore_m)
    in
    let%bind () =
      Deferred.List.iter (List.init 5 ~f:Fn.id) ~how:`Parallel ~f:(fun i ->
        let participant = Participant.of_string [%string "Trader%{i#Int}"] in
        let%bind client = connect_as ~port participant in
        rpc_submit client (Harness.buy ~price_cents:15010 ~participant ())
        |> Deferred.ignore_m)
    in
    (* Each client's session feed flushes its events to stdout in an order
       that depends on which parallel buy was processed first. Swallow the
       trace and assert on the deterministic remaining book state instead: 10
       sells went in, the 5 buys at $150.10 each hit the lowest-priced sell,
       so 5 sells should remain. *)
    let (_ : string) = [%expect.output] in
    let%bind book = rpc_book seed Harness.aapl in
    let book = Option.value_exn book in
    let remaining_orders = List.length book.bids + List.length book.asks in
    [%test_result: int] remaining_orders ~expect:5;
    return ())
;;

(* ---------------------------------------------------------------- *)
(* Audit log subscription tests *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: audit log subscriber sees full unfiltered stream \
                 across symbols"
  =
  with_server ~symbols:[ Harness.aapl; Harness.tsla ] (fun ~server:_ ~port ->
    let%bind sub = connect_as ~port (Participant.of_string "Auditor") in
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind result =
      Rpc.Pipe_rpc.dispatch Rpc_protocol.audit_log_rpc (connection sub) ()
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[AUDIT] %{e}"]));
    (* Post a sell on AAPL — audit subscriber should see ACCEPTED and BBO. *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell ~price_cents:15000 ~participant:Harness.bob ())
    in
    [%expect
      {|
      [AUDIT] ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
      [AUDIT] BBO AAPL bid=- ask=$150.00 x100
      [Bob] ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
      |}];
    (* Post a sell on TSLA — audit subscriber should see this too
       (multi-symbol). *)
    let tsla_request =
      Harness.sell
        ~price_cents:20000
        ~symbol:Harness.tsla
        ~participant:Harness.bob
        ()
    in
    let tsla_with_id =
      { tsla_request with client_order_id = Client_order_id.of_int 2 }
    in
    let%bind () = rpc_submit bob tsla_with_id in
    [%expect
      {|
      [AUDIT] ACCEPTED id=2 TSLA SELL 100@$200.00 DAY
      [AUDIT] BBO TSLA bid=- ask=$200.00 x100
      [Bob] ACCEPTED id=2 TSLA SELL 100@$200.00 DAY
    |}];
    (* Cross the AAPL sell — the audit log should see ACCEPTED + FILL + BBO. *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [AUDIT] ACCEPTED id=3 AAPL BUY 100@$150.00 DAY
      [AUDIT] FILL fill_id=1 AAPL $150.00 x100 aggressor=3(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [AUDIT] TRADE AAPL $150.00 x100
      [AUDIT] BBO AAPL bid=- ask=-
      [Alice] ACCEPTED id=3 AAPL BUY 100@$150.00 DAY
      [Alice] FILL fill_id=1 AAPL $150.00 x100 aggressor=3(Alice, cid=1) BUY resting=1(Bob, cid=1)
      [Bob] FILL fill_id=1 AAPL $150.00 x100 aggressor=3(Alice, cid=1) BUY resting=1(Bob, cid=1)
      |}];
    return ())
;;

let%expect_test "dispatcher: closing a subscriber's reader removes the \
                 writer"
  =
  let dispatcher = Dispatcher.create () in
  print_s
    [%message
      "initial"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| (initial (count 0)) |}];
  let reader_a = Dispatcher.subscribe_audit dispatcher in
  let reader_b = Dispatcher.subscribe_audit dispatcher in
  print_s
    [%message
      "after subscribe"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| ("after subscribe" (count 2)) |}];
  Pipe.close_read reader_a;
  let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
  print_s
    [%message
      "after closing reader_a"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| ("after closing reader_a" (count 1)) |}];
  Pipe.close_read reader_b;
  let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
  print_s
    [%message
      "after closing reader_b"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| ("after closing reader_b" (count 0)) |}];
  return ()
;;

let%expect_test "login required before submit or cancel" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let target_address =
      Tcp.Where_to_connect.of_host_and_port
        { Host_and_port.host = "localhost"; port }
    in
    let%bind raw_conn1 =
      Rpc.Connection.client target_address
      |> Deferred.Result.map_error ~f:Error.of_exn
      |> Deferred.Or_error.ok_exn
    in
    let%bind raw_conn2 =
      Rpc.Connection.client target_address
      |> Deferred.Result.map_error ~f:Error.of_exn
      |> Deferred.Or_error.ok_exn
    in
    let request = Harness.buy ~price_cents:15000 () in
    let%bind sub_res =
      Rpc.Rpc.dispatch Rpc_protocol.submit_order_rpc raw_conn1 request
    in
    print_endline
      (match sub_res with
       | Ok (Error err) -> Error.to_string_hum err
       | _ -> "Unexpected success");
    [%expect {| Not logged in |}];
    let%bind cancel_res =
      Rpc.Rpc.dispatch
        Rpc_protocol.cancel_order_rpc
        raw_conn2
        (Client_order_id.of_int 1)
    in
    print_endline
      (match cancel_res with
       | Ok (Error err) -> Error.to_string_hum err
       | _ -> "Unexpected success");
    [%expect {| Not logged in |}];
    let%bind () = Rpc.Connection.close raw_conn1 in
    let%bind () = Rpc.Connection.close raw_conn2 in
    return ())
;;

let%expect_test "dual login conflicts block second participant connection" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind _alice1 = connect_as ~port Harness.alice in
    let target_address =
      Tcp.Where_to_connect.of_host_and_port
        { Host_and_port.host = "localhost"; port }
    in
    let%bind raw_conn =
      Rpc.Connection.client target_address
      |> Deferred.Result.map_error ~f:Error.of_exn
      |> Deferred.Or_error.ok_exn
    in
    let%bind login_res =
      Rpc.Rpc.dispatch
        Rpc_protocol.login_rpc
        raw_conn
        (Participant.to_string Harness.alice)
    in
    print_endline
      (match login_res with
       | Ok (Error err) -> Error.to_string_hum err
       | _ -> "Unexpected success");
    [%expect {| Conflict: Participant is already logged |}];
    let%bind () = Rpc.Connection.close raw_conn in
    return ())
;;

let%expect_test "second login on the same connection is rejected" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let target_address =
      Tcp.Where_to_connect.of_host_and_port
        { Host_and_port.host = "localhost"; port }
    in
    let connect () =
      Rpc.Connection.client target_address
      |> Deferred.Result.map_error ~f:Error.of_exn
      |> Deferred.Or_error.ok_exn
    in
    let login conn name =
      Rpc.Rpc.dispatch
        Rpc_protocol.login_rpc
        conn
        (Participant.to_string name)
    in
    let print_login label res =
      print_endline
        (match res with
         | Ok (Ok p) -> [%string "%{label}: logged in as %{p#Participant}"]
         | Ok (Error err) -> [%string "%{label}: %{Error.to_string_hum err}"]
         | Error err ->
           [%string "%{label}: transport error: %{Error.to_string_hum err}"])
    in
    let%bind conn1 = connect () in
    let%bind first = login conn1 Harness.alice in
    print_login "alice on conn1" first;
    [%expect {| alice on conn1: logged in as Alice |}];
    (* A second login on the SAME connection must be rejected, not silently
       overwrite alice's session — doing so would orphan alice in the
       registry (disconnect only cleans up the connection's current session). *)
    let%bind second = login conn1 Harness.bob in
    print_login "bob on conn1" second;
    [%expect {| bob on conn1: Already logged in on this connection |}];
    (* The rejected login must not have half-registered bob: a fresh
       connection can still claim the name. *)
    let%bind conn2 = connect () in
    let%bind third = login conn2 Harness.bob in
    print_login "bob on conn2" third;
    [%expect {| bob on conn2: logged in as Bob |}];
    let%bind () = Rpc.Connection.close conn1 in
    let%bind () = Rpc.Connection.close conn2 in
    return ())
;;

let%expect_test "submit, cancel, BBO update delta shift, duplicate rejects, \
                 non-existent drops"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind bob = connect_as ~port Harness.bob in
    (* 1. resting order *)
    let req1 =
      { (Harness.sell ~price_cents:15000 ~participant:Harness.bob ()) with
        client_order_id = Client_order_id.of_int 100
      }
    in
    let%bind () = rpc_submit bob req1 in
    [%expect {| 
      [Bob] ACCEPTED id=1 AAPL SELL 100@$150.00 DAY 
    |}];
    (* 2. Verify duplicate Client Order ID submission fails *)
    let req_dup =
      { (Harness.sell ~price_cents:15100 ~participant:Harness.bob ()) with
        client_order_id = Client_order_id.of_int 100
      }
    in
    let%bind () = rpc_submit bob req_dup in
    [%expect
      {| 
      [Bob] REJECTED AAPL SELL 100@$151.00 reason=duplicate client_order_id 
    |}];
    (* 3. Cancel order by ID and verify session feed receives Order_cancel
       and BBO update events *)
    let%bind result =
      Rpc.Rpc.dispatch
        Rpc_protocol.cancel_order_rpc
        (connection bob)
        (Client_order_id.of_int 100)
    in
    let () =
      Or_error.ok_exn
        (match result with Ok res -> res | Error err -> Error.raise err)
    in
    (* Yield scheduler execution to allow synchronous dispatcher queues to
       drain into the network pipe *)
    let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
    [%expect
      {| [Bob] CANCELLED id=1 AAPL remaining=100 reason=PARTICIPANT_REQUESTED cid=100 |}];
    (* 4. Cancel a non-existent order *)
    let%bind result_missing =
      Rpc.Rpc.dispatch
        Rpc_protocol.cancel_order_rpc
        (connection bob)
        (Client_order_id.of_int 999)
    in
    let () =
      Or_error.ok_exn
        (match result_missing with
         | Ok res -> res
         | Error err -> Error.raise err)
    in
    let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [Bob] CANCEL_REJECT cl_ord_id=999 reason=order not found
    |}];
    return ())
;;

let%expect_test "e2e: cross matching triggers fill events on resting \
                 session feed and blocks cancellation of filled positions"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind bob = connect_as ~port Harness.bob in
    let%bind alice = connect_as ~port Harness.alice in
    (* 1. Bob places a resting sell order *)
    let bob_req =
      { (Harness.sell ~price_cents:15000 ~participant:Harness.bob ()) with
        client_order_id = Client_order_id.of_int 500
      }
    in
    let%bind () = rpc_submit bob bob_req in
    [%expect {| [Bob] ACCEPTED id=1 AAPL SELL 100@$150.00 DAY |}];
    (* 2. Alice aggressive buy order *)
    let alice_req =
      { (Harness.buy ~price_cents:15000 ~participant:Harness.alice ()) with
        client_order_id = Client_order_id.of_int 1
      }
    in
    let%bind () = rpc_submit alice alice_req in
    [%expect
      {|
      [Alice] ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
      [Alice] FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=500)
      [Bob] FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice, cid=1) BUY resting=1(Bob, cid=500)
    |}];
    (* 3. Bob attempts to cancel the fully filled order. trigger a "not
       found" reject because filled orders are removed from the book. *)
    let%bind result_filled =
      Rpc.Rpc.dispatch
        Rpc_protocol.cancel_order_rpc
        (connection bob)
        (Client_order_id.of_int 500)
    in
    let () =
      Or_error.ok_exn
        (match result_filled with
         | Ok res -> res
         | Error err -> Error.raise err)
    in
    let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [Bob] CANCEL_REJECT cl_ord_id=500 reason=order not found
    |}];
    return ())
;;
