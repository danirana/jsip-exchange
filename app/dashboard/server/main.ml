open! Core
open! Async
open Jsip_types
module Protocol = Jsip_dashboard_protocol
module Bot_kind = Protocol.Bot_kind
module Bot_spec = Jsip_scenario_runner.Bot_spec
module Runner = Jsip_scenario_runner.Runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

(* The dashboard server bridges the exchange to the browser. It holds one TCP
   connection to the exchange, draining its [stats_rpc] pipe into a rolling
   window, and runs one HTTP/websocket server that (a) serves the compiled
   Bonsai client, (b) answers [recent_stats_rpc] polls with the current
   window, and (c) launches a bot against the exchange on [launch_bot_rpc].
   Async is cooperatively scheduled on one thread, so the plain [ref]s below
   need no locking. *)

let window = ref Protocol.Window.empty

(* One monotonic counter shared by every launch, so repeated clicks mint
   distinct participant names (a duplicate login is rejected by the exchange)
   and distinct oracle/RNG seeds. *)
let launch_counter = ref 0

(* Every bot this dashboard has launched and is still tracking, keyed by the
   participant it trades as. [stop] is the kill switch returned by
   {!Runner.start_bot}: halt the loop, flatten its orders, disconnect. *)
type running_bot =
  { kind : Bot_kind.t
  ; stop : unit -> unit Deferred.t
  }

let running : running_bot Participant.Table.t = Participant.Table.create ()

(* Fundamental fallback when a symbol has no touch to read a mid from — a
   bare book (no bid and no ask). Only used to seed a fresh bot's oracle. *)
let fallback_price_cents = 10_000

(* Connect to the exchange and forward every per-second snapshot into
   [window]. Runs for the life of the process. *)
let start_draining_exchange ~where_to_connect =
  let%bind connection =
    match%map Rpc.Connection.client where_to_connect with
    | Ok connection -> connection
    | Error exn ->
      raise_s
        [%message "dashboard: could not connect to exchange" (exn : Exn.t)]
  in
  let%map pipe =
    match%map
      Rpc.Pipe_rpc.dispatch Jsip_gateway.Rpc_protocol.stats_rpc connection ()
    with
    | Ok (Ok (pipe, (_ : Rpc.Pipe_rpc.Metadata.t))) -> pipe
    | Ok (Error error) | Error error ->
      raise_s [%message "dashboard: stats_rpc failed" (error : Error.t)]
  in
  don't_wait_for
    (Pipe.iter_without_pushback pipe ~f:(fun snapshot ->
       window := Protocol.Window.add !window snapshot))
;;

(* The exchange's live symbols paired with a fundamental seed for each, read
   off the newest snapshot. A launched bot trades exactly the symbols the
   exchange reports, priced around the current mid so its orders land near
   the touch instead of miles off-book. Empty until the first snapshot
   arrives. *)
let market_seed () : (Symbol.t * int) list =
  let { Protocol.Window.samples; _ } = !window in
  match List.last samples with
  | None -> []
  | Some snapshot ->
    List.map
      snapshot.book_depth
      ~f:(fun (depth : Exchange_stats.Book_depth.t) ->
        let mid_cents =
          match
            Bbo.price depth.bbo Side.Buy, Bbo.price depth.bbo Side.Sell
          with
          | Some bid, Some ask ->
            (Price.to_int_cents bid + Price.to_int_cents ask) / 2
          | Some one_side, None | None, Some one_side ->
            Price.to_int_cents one_side
          | None, None -> fallback_price_cents
        in
        depth.symbol, mid_cents)
;;

(* A single-symbol oracle process seeded near the live market, so the bot's
   [Context.fundamental] tracks a price close to the real touch. The tuning
   knobs mirror the scenarios' defaults. *)
let make_oracle ~seed (seeds : (Symbol.t * int) list) : Fundamental_oracle.t =
  let symbol_config initial_price_cents
    : Fundamental_oracle.Config.symbol_config
    =
    { initial_price_cents
    ; volatility_cents_per_sec = 5.0
    ; mean_reversion_strength = 0.1
    ; tick_interval = Time_ns.Span.of_sec 0.5
    }
  in
  let config =
    Symbol.Map.of_alist_exn
      (List.map seeds ~f:(fun (symbol, cents) -> symbol, symbol_config cents))
  in
  Fundamental_oracle.create config ~seed
;;

let default_tick_interval = Time_ns.Span.of_sec 1.0

(* Map a chosen bot kind to a runnable spec, with sensible default knobs.
   [symbols] is the live symbol set; each bot trades all of them. The config
   constants here are the intensity dials — the same ones the scenarios in
   [app/scenarios] tune. *)
let spec_of_kind (kind : Bot_kind.t) ~symbols ~participant ~rng_seed
  : Bot_spec.t
  =
  match kind with
  | Noise_trader ->
    let config =
      Jsip_bots.Noise_trader.Config.create
        ~symbols
        ~orders_per_tick:5
        ~jitter_cents:10
        ~size:20
        ()
    in
    Bot_spec.T
      { bot = (module Jsip_bots.Noise_trader)
      ; config
      ; participant
      ; symbols
      ; rng_seed
      ; tick_interval = default_tick_interval
      ; is_marketdata_consumer = false
      }
  | Book_filler ->
    let config =
      Jsip_bots.Book_filler.Config.create
        ~symbols
        ~orders_per_tick:20
        ~size:50
        ~min_offset_cents:5
        ~max_offset_cents:50
        ()
    in
    Bot_spec.T
      { bot = (module Jsip_bots.Book_filler)
      ; config
      ; participant
      ; symbols
      ; rng_seed
      ; tick_interval = default_tick_interval
      ; is_marketdata_consumer = false
      }
  | Cancel_storm ->
    let config =
      Jsip_bots.Cancel_storm.Config.create
        ~symbols
        ~cycles_per_tick:20
        ~max_in_flight:50
        ~size:10
        ~passive_offset_cents:20
        ()
    in
    Bot_spec.T
      { bot = (module Jsip_bots.Cancel_storm)
      ; config
      ; participant
      ; symbols
      ; rng_seed
      ; tick_interval = default_tick_interval
      ; is_marketdata_consumer = false
      }
  | Spammer ->
    let config : Jsip_bots.Spammer.Config.t =
      { symbols
      ; orders_per_tick = 50
      ; size = 10
      ; next_client_order_id = ref 1
      }
    in
    Bot_spec.T
      { bot = (module Jsip_bots.Spammer)
      ; config
      ; participant
      ; symbols
      ; rng_seed
      ; tick_interval = default_tick_interval
      ; is_marketdata_consumer = false
      }
  | Slow_consumer ->
    let config =
      Jsip_bots.Slow_consumer.Config.create
        ~read_behavior:
          (Jsip_bots.Slow_consumer.Read_behavior.Delay_per_event
             (Time_ns.Span.of_ms 200.))
    in
    Bot_spec.T
      { bot = (module Jsip_bots.Slow_consumer)
      ; config
      ; participant
      ; symbols
      ; rng_seed
      ; tick_interval = default_tick_interval
      ; is_marketdata_consumer = true
      }
;;

(* Handle one [launch_bot_rpc]: figure out what the exchange is trading,
   build the bot, and start it. Fails cleanly (not by raising across the RPC)
   when there's no market to trade yet or the bring-up throws. *)
let launch ~where_to_connect (kind : Bot_kind.t) : unit Or_error.t Deferred.t
  =
  match market_seed () with
  | [] ->
    return
      (Or_error.error_string
         "no market data yet — wait for the exchange to report symbols, \
          then launch again")
  | seeds ->
    incr launch_counter;
    let n = !launch_counter in
    let symbols = List.map seeds ~f:fst in
    let participant =
      Participant.of_string
        [%string "%{Bot_kind.to_value_string kind}-%{n#Int}"]
    in
    let oracle = make_oracle ~seed:n seeds in
    don't_wait_for (Fundamental_oracle.start oracle);
    let spec = spec_of_kind kind ~symbols ~participant ~rng_seed:n in
    (match%map
       Monitor.try_with (fun () ->
         Runner.start_bot ~where_to_connect ~oracle spec)
     with
     | Ok stop ->
       Hashtbl.set running ~key:participant ~data:{ kind; stop };
       Ok ()
     | Error exn ->
       Or_error.error_s [%message "bot launch failed" (exn : Exn.t)])
;;

(* Stop one tracked bot: flatten and disconnect it, then forget it. *)
let stop_bot (participant : Participant.t) : unit Or_error.t Deferred.t =
  match Hashtbl.find running participant with
  | None ->
    return
      (Or_error.error_s
         [%message "no such running bot" (participant : Participant.t)])
  | Some { stop; kind = _ } ->
    let%map () = stop () in
    Hashtbl.remove running participant;
    Ok ()
;;

(* Full reset: stop every tracked bot in parallel and forget them all. *)
let reset_bots () : unit Or_error.t Deferred.t =
  let bots = Hashtbl.data running in
  Hashtbl.clear running;
  let%map () =
    Deferred.List.iter ~how:`Parallel bots ~f:(fun { stop; kind = _ } ->
      stop ())
  in
  Ok ()
;;

(* The bots still tracked, sorted by participant so the client's list is
   stable frame to frame. *)
let running_bots () : Protocol.Running_bot.t list =
  Hashtbl.to_alist running
  |> List.map ~f:(fun (participant, { kind; stop = _ }) ->
    { Protocol.Running_bot.participant; kind })
  |> List.sort ~compare:(fun a b ->
    Participant.compare a.participant b.participant)
;;

(* Full exchange wipe: stop our bots, then ask the exchange to flatten its
   entire book (seed market maker and leftover junk included). We open a
   throwaway connection for the one operator RPC rather than reuse the stats
   drain connection. *)
let reset_exchange ~where_to_connect () : unit Or_error.t Deferred.t =
  let%bind (_ : unit Or_error.t) = reset_bots () in
  match%bind Rpc.Connection.client where_to_connect with
  | Error exn ->
    return
      (Or_error.error_s
         [%message "reset: could not connect to exchange" (exn : Exn.t)])
  | Ok connection ->
    let%bind dispatched =
      Rpc.Rpc.dispatch
        Jsip_gateway.Rpc_protocol.reset_exchange_rpc
        connection
        ()
    in
    let%map () = Rpc.Connection.close connection in
    (match dispatched with Ok result -> result | Error error -> Error error)
;;

let implementations ~where_to_connect =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement Protocol.Rpcs.recent_stats_rpc (fun () () ->
          return !window)
      ; Rpc.Rpc.implement Protocol.Rpcs.launch_bot_rpc (fun () kind ->
          launch ~where_to_connect kind)
      ; Rpc.Rpc.implement Protocol.Rpcs.stop_bot_rpc (fun () participant ->
          stop_bot participant)
      ; Rpc.Rpc.implement Protocol.Rpcs.reset_bots_rpc (fun () () ->
          reset_bots ())
      ; Rpc.Rpc.implement Protocol.Rpcs.reset_exchange_rpc (fun () () ->
          reset_exchange ~where_to_connect ())
      ; Rpc.Rpc.implement Protocol.Rpcs.running_bots_rpc (fun () () ->
          return (running_bots ()))
      ]
    ~on_unknown_rpc:`Close_connection
    ~on_exception:Log_on_background_exn
;;

let respond ~content_type body =
  Cohttp_async.Server.respond_string
    ~headers:(Cohttp.Header.of_list [ "content-type", content_type ])
    body
;;

(* Non-websocket HTTP requests land here: serve the page and its JS bundle,
   both embedded in this binary so [dune exec] works from any directory. A
   websocket upgrade to any path is intercepted before this by [serve] and
   becomes the RPC connection. *)
let http_handler
  ()
  ~body:(_ : Cohttp_async.Body.t)
  (_ : Socket.Address.Inet.t)
  (request : Cohttp_async.Request.t)
  =
  match Uri.path (Cohttp.Request.uri request) with
  | "/" | "/index.html" ->
    respond
      ~content_type:"text/html; charset=utf-8"
      Embedded_files.index_html
  | "/main.bc.js" ->
    respond ~content_type:"text/javascript" Embedded_files.main_js
  | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found"
;;

let serve ~http_port ~exchange_host ~exchange_port =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port
      { host = exchange_host; port = exchange_port }
  in
  let%bind () = start_draining_exchange ~where_to_connect in
  let%bind (_ : (Socket.Address.Inet.t, int) Cohttp_async.Server.t) =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port http_port)
      ~implementations:(implementations ~where_to_connect)
      ~initial_connection_state:(fun () _from _addr _conn -> ())
      ~http_handler
      ()
  in
  Core.printf "dashboard on http://localhost:%d\n%!" http_port;
  Deferred.never ()
;;

let command =
  Command.async
    ~summary:"jsip exchange health dashboard (bonsai_web)"
    (let%map_open.Command http_port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT HTTP port to serve the dashboard on (default 8080)"
     and exchange_host =
       flag
         "-exchange-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST exchange host (default localhost)"
     and exchange_port =
       flag
         "-exchange-port"
         (optional_with_default 12345 int)
         ~doc:"PORT exchange RPC port (default 12345)"
     in
     fun () -> serve ~http_port ~exchange_host ~exchange_port)
;;

let () = Command_unix.run command
