(** Exchange server.

    Runs the matching engine and listens for RPC connections from clients.

    Run with: dune exec app/server/bin/main.exe -- -port 12345

    Optionally seed the book with a market maker: dune exec
    app/server/bin/main.exe -- -port 12345 -seed-market-maker *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway
module Bot_runtime = Jsip_bot_runtime.Bot_runtime
module Market_maker_bot = Jsip_market_maker.Market_maker_bot
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

let default_symbols =
  [ Symbol.of_string "AAPL"
  ; Symbol.of_string "TSLA"
  ; Symbol.of_string "GOOG"
  ; Symbol.of_string "MSFT"
  ]
;;

let connect_as ~where_to_connect participant =
  let%bind connection =
    Rpc.Connection.client where_to_connect >>| Result.ok_exn
  in
  let username = Participant.to_string participant in
  let%map login_result =
    Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc connection username
  in
  let _authenticated_participant = Or_error.ok_exn login_result in
  connection
;;

(* Seed the book with a single dynamic market maker on AAPL, driven through
   [Bot_runtime]: open a logged-in connection, hand the runtime [submit] /
   [cancel] closures bound to that connection, and feed the bot's session
   events into its [on_event] handler. This mirrors
   [Jsip_scenario_runner.Runner.start_bot]. *)
let seed_market_maker ~where_to_connect =
  let mm_participant = Participant.of_string "MarketMaker" in
  let%bind connection = connect_as ~where_to_connect mm_participant in
  let config =
    Market_maker_bot.Config.create
    (* Symbol id 0 — AAPL, the first entry in [default_symbols]. In phase 1
       the exchange speaks ids, so the seed maker trades by id, not name. *)
      ~symbol:(Symbol_id.of_int 0)
      ~fair_value_cents:15000
      ~half_spread_cents:10
      ~size_per_level:100
      ~num_levels:5
      ~client_order_id:(Client_order_id.of_int 1)
      ~inventory_skew_cents_per_share:5
  in
  let submit request =
    Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc connection request
  in
  let cancel client_order_id =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.cancel_order_rpc
      connection
      client_order_id
  in
  (* This bot never consults the fundamental oracle, but [Bot_runtime.create]
     requires one; an empty oracle suffices. *)
  let oracle =
    Fundamental_oracle.create
      (Symbol_id.Map.empty : Fundamental_oracle.Config.t)
      ~seed:0
  in
  let bot =
    Bot_runtime.create
      (module Market_maker_bot : Bot_runtime.Bot
        with type Config.t = Market_maker_bot.Config.t)
      config
      ~participant:mm_participant
      ~oracle
      ~rng:(Splittable_random.of_int 0)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.)
  in
  let%bind session_pipe, session_metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc connection ()
  in
  don't_wait_for (Pipe.iter session_pipe ~f:(Bot_runtime.feed_event bot));
  don't_wait_for
    (match%map Rpc.Pipe_rpc.close_reason session_metadata with
     | Rpc.Pipe_close_reason.Closed_locally
     | Rpc.Pipe_close_reason.Closed_remotely ->
       ()
     | Rpc.Pipe_close_reason.Error err ->
       [%log.error "session feed pipe closed with error" (err : Error.t)]);
  (* The seed market maker runs for the life of the server; it is never
     stopped. *)
  don't_wait_for (Bot_runtime.start ~stop:(Deferred.never ()) bot);
  return ()
;;

let start ~port ~should_seed_market_maker =
  (* [default_symbols] defines the id order (its [i]th name is id [i]). The
     authoritative directory is built here, once, and passed into the server;
     it is what the directory RPC serves so clients can show and accept names
     while ids still travel on the wire. *)
  let directory =
    Symbol_directory.create
      (List.mapi default_symbols ~f:(fun i name -> name, Symbol_id.of_int i))
  in
  let%bind server = Exchange_server.start ~directory ~port () in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind () =
    if should_seed_market_maker
    then (
      print_endline "=== Seeding book with market maker orders ===";
      seed_market_maker ~where_to_connect)
    else Deferred.unit
  in
  print_endline
    [%string
      "JSIP Exchange server listening on port %{Exchange_server.port \
       server#Int}"];
  let symbols =
    List.map default_symbols ~f:Symbol.to_string |> String.concat ~sep:", "
  in
  print_endline [%string "Trading: %{symbols}"];
  Exchange_server.close_finished server
;;

let () =
  Command.async
    ~summary:"JSIP Exchange server"
    (let%map_open.Command port =
       flag "-port" (required int) ~doc:"PORT port to listen on"
     and should_seed_market_maker =
       flag
         "-seed-market-maker"
         no_arg
         ~doc:" seed the book with a dynamic market maker bot"
     in
     fun () -> start ~port ~should_seed_market_maker)
  |> Command_unix.run
;;
