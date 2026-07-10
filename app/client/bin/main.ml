(** Exchange client.

    Connects to a running exchange server and provides an interactive
    command-line interface for submitting orders and querying the book.

    Run with: dune exec app/client/bin/main.exe -- -host localhost -port
    12345 -name Alice *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway

let run_client ~host ~port ~participant_name =
  let participant = Participant.of_string participant_name in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  let%bind conn = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
  (* Log in before any session-scoped RPC: the server rejects session-feed,
     book, and order requests from a connection with no session. *)
  let%bind login_result =
    Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc conn participant_name
  in
  let (_ : Participant.t) = Or_error.ok_exn login_result in
  (* Fetch the symbol directory once and keep a local mirror, so we can
     accept names at the prompt and show names in output while the wire still
     carries ids. [render_symbol] is id->name for display; [resolve_symbol]
     is name->id for parsing. *)
  let%bind directory_alist =
    Rpc.Rpc.dispatch_exn Rpc_protocol.symbol_directory_rpc conn ()
  in
  let directory = Symbol_directory.create directory_alist in
  let render_symbol = Symbol_directory.label directory in
  let resolve_symbol = Symbol_directory.id_of_name directory in
  (* open a pipe between client and server *)
  let%bind session_feed_result =
    Rpc.Pipe_rpc.dispatch Rpc_protocol.session_feed_rpc conn ()
  in
  match session_feed_result with
  | Error err ->
    print_endline [%string "ERROR: %{Error.to_string_hum err}"];
    return (Error err)
  | Ok (Error err) ->
    print_endline [%string "ERROR: %{Error.to_string_hum err}"];
    return (Error err) (* pipe reader returned *)
  | Ok (Ok (reader, _id)) ->
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         match event with
         | Exchange_event.Fill fill ->
           (match
              Protocol.fill_participant_view
                ~render_symbol
                fill
                ~viewer:participant
            with
            | Some view_str -> print_endline view_str
            | None -> ())
         | Exchange_event.Order_accept { order_id; _ } ->
           print_endline
             [%string
               "[SESSION] Order accepted: %{Order_id.to_string order_id}"]
         | Exchange_event.Order_cancel { order_id; _ } ->
           print_endline
             [%string
               "[SESSION] Order canceled: %{Order_id.to_string order_id}"]
         | Exchange_event.Order_reject { reason; _ } ->
           print_endline [%string "[SESSION] Order rejected: %{reason}"]
         | Exchange_event.Best_bid_offer_update _
         | Exchange_event.Trade_report _ ->
           ()
         | Exchange_event.Cancel_reject { client_order_id; reason; _ } ->
           print_endline
             [%string
               "[SESSION] Cancel rejected for ID \
                %{Client_order_id.to_string client_order_id}: %{reason}"]));
    print_endline
      [%string
        "Connected to exchange at %{host}:%{port#Int} as \
         %{participant#Participant}\n\
         Commands: BUY|SELL <client_order_id> <symbol> <size> <price> \
         [%{Time_in_force.all_str}]\n\
        \          BOOK <symbol>\n\
        \          SUBSCRIBE <symbol>  (stream market data)\n\n\
         Order acknowledgements, fills, and cancellations are temporarily \
         printed\n\
         by the server process; the SUBSCRIBE command attaches you to a \
         per-symbol\n\
         market-data feed."];
    let rec loop () =
      print_string "> ";
      match%bind Reader.read_line (Lazy.force Reader.stdin) with
      | `Eof ->
        print_endline "\nDisconnected.";
        Deferred.Or_error.ok_unit
      | `Ok line ->
        let line = String.strip line in
        if String.is_empty line
        then loop ()
        else (
          match
            Exchange_command.parse
              ~default_participant:participant
              ~resolve_symbol
              line
          with
          | Error msg ->
            print_endline [%string "ERROR: %{Error.to_string_hum msg}"];
            loop ()
          | Ok (Book symbol) ->
            let%bind result =
              Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc conn symbol
            in
            (match result with
             | None ->
               let label = render_symbol symbol in
               print_endline [%string "No book available for %{label}"]
             | Some result ->
               print_endline (Protocol.format_book ~render_symbol result));
            loop ()
          | Ok (Subscribe symbol) ->
            let%bind result =
              Rpc.Pipe_rpc.dispatch
                Rpc_protocol.market_data_rpc
                conn
                [ symbol ]
            in
            (match result with
             | Error err | Ok (Error err) ->
               print_endline
                 [%string "ERROR subscribing: %{Error.to_string_hum err}"];
               loop ()
             | Ok (Ok (reader, _id)) ->
               let label = render_symbol symbol in
               print_endline
                 [%string
                   {| Subscribed to %{label} market data. Updates will appear below.
Continue entering commands as normal.|}];
               don't_wait_for
                 (Pipe.iter_without_pushback reader ~f:(fun event ->
                    let line = Protocol.format_event ~render_symbol event in
                    print_endline [%string "[MD] %{line}"]));
               loop ())
          | Ok (Cancel cancel_request) ->
            let%bind.Deferred.Or_error () =
              Rpc.Rpc.dispatch_exn
                Rpc_protocol.cancel_order_rpc
                conn
                cancel_request
            in
            loop ()
          | Ok (Submit request) ->
            let%bind.Deferred.Or_error () =
              Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
            in
            loop ())
    in
    loop ()
;;

let () =
  Command.async_or_error
    ~summary:"JSIP Exchange client"
    (let%map_open.Command host =
       flag
         "-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST server hostname"
     and port = flag "-port" (required int) ~doc:"PORT server port"
     and participant_name =
       flag
         "-name"
         (optional_with_default (Core_unix.getlogin ()) string)
         ~doc:"NAME participant name"
     in
     fun () -> run_client ~host ~port ~participant_name)
  |> Command_unix.run
;;
