open! Core
open! Async
open Jsip_types
open Jsip_order_book

(* A submitted request tagged with when it entered the server. The submit RPC
   only enqueues; the matching engine handles the request later, off the
   queue. Carrying [enqueued_at] through the pipe lets the matching loop
   measure the whole span — time spent waiting behind other work, plus the
   match itself — which is exactly the submit latency the dashboard cares
   about under load. *)
module Queued_request = struct
  type t =
    { request : Order.Request.t
    ; enqueued_at : Time_ns.t
    }
end

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : Queued_request.t Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  }

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let participant t = Option.map t.session ~f:Session.participant
end

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_submit ~request_writer (request : Order.Request.t) =
  (* Stamp on entry — this handler runs when the client calls the RPC, so
     [now] is the closest we get to "client submitted". *)
  let queued = { Queued_request.request; enqueued_at = Time_ns.now () } in
  let%map () = Pipe.write_if_open request_writer queued in
  Ok ()
;;

let start_matching_loop ~engine ~dispatcher ~metrics request_reader =
  (* Time between successive iterations of this loop, for the engine-busyness
     metric. [None] until the first iteration so we don't record a bogus gap
     from process start to the first order. *)
  let previous_iteration = ref None in
  don't_wait_for
    (Pipe.iter_without_pushback
       request_reader
       ~f:(fun { Queued_request.request; enqueued_at } ->
         let now = Time_ns.now () in
         (match !previous_iteration with
          | Some previous ->
            Metrics.record_engine_gap metrics (Time_ns.diff now previous)
          | None -> ());
         previous_iteration := Some now;
         Metrics.record_order metrics request.participant;
         let events = Matching_engine.submit engine request in
         (* Record before dispatching: the latency we want is up to "engine
            handled it", not the routing of the resulting events. *)
         Metrics.record_submit_latency
           metrics
           (Time_ns.diff (Time_ns.now ()) enqueued_at);
         Dispatcher.dispatch dispatcher events))
;;

let start ?market_data_budget ?session_budget ?audit_budget ~symbols ~port ()
  =
  let engine = Matching_engine.create symbols in
  let dispatcher =
    Dispatcher.create ?market_data_budget ?session_budget ?audit_budget ()
  in
  let metrics = Metrics.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher ~metrics request_reader;
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun state request ->
               match Connection_state.participant state with
               | None -> return (Or_error.error_string "Not logged in")
               | Some new_participant ->
                 let new_request =
                   { request with
                     Order.Request.participant = new_participant
                   }
                 in
                 handle_submit ~request_writer new_request)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun state () ->
               match state.Connection_state.session with
               | None -> return (Error (Error.of_string "not logged in"))
               | Some session ->
                 let reader = Session.reader session in
                 return (Ok reader))
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun state symbols ->
               ignore state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.audit_log_rpc (fun state () ->
            ignore state;
            let reader = Dispatcher.subscribe_audit dispatcher in
            return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.stats_rpc (fun state () ->
            ignore state;
            let reader = Metrics.subscribe metrics in
            return (Ok reader))
        ; Rpc.Rpc.implement Rpc_protocol.login_rpc (fun state name ->
            let all_whitespace str =
              let is_whitespace = function
                | ' ' | '\x0C' | '\n' | '\r' | '\t' -> true
                | _ -> false
              in
              String.for_all str ~f:is_whitespace
            in
            if all_whitespace name || String.is_empty name
            then return (Or_error.error_string "Not valid name")
            else (
              match state.Connection_state.session with
              | Some _ ->
                (* One session per connection. A second login on the same
                   connection would orphan the first participant in the
                   registry, because disconnect cleanup only removes the
                   connection's current [state.session]. *)
                return
                  (Or_error.error_string
                     "Already logged in on this connection")
              | None ->
                let participant = Participant.of_string name in
                let table = Dispatcher.sessions dispatcher in
                if Hashtbl.mem table participant
                then
                  return
                    (Or_error.error_string
                       "Conflict: Participant is already logged")
                else (
                  let new_session =
                    Dispatcher.create_session dispatcher participant
                  in
                  Hashtbl.set table ~key:participant ~data:new_session;
                  state.Connection_state.session <- Some new_session;
                  return (Ok participant))))
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun state client_order_id ->
               match Connection_state.participant state with
               | None -> return (Or_error.error_string "Not logged in")
               | Some participant ->
                 (* Cancel is handled synchronously in this handler (no
                    queue), so its latency is just the engine call — no
                    enqueue span to add, unlike submit. *)
                 let started_at = Time_ns.now () in
                 let events =
                   Matching_engine.cancel engine participant client_order_id
                 in
                 Metrics.record_cancel_latency
                   metrics
                   (Time_ns.diff (Time_ns.now ()) started_at);
                 Dispatcher.dispatch dispatcher events;
                 return (Ok ()))
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_all_orders_rpc
            (fun state () ->
               match Connection_state.participant state with
               | None -> return (Or_error.error_string "Not logged in")
               | Some participant ->
                 (* Flatten the whole participant in one shot, then fan the
                    resulting cancels out to subscribers like any other
                    cancel. No latency is recorded: a mass-cancel is an
                    operator action, not a per-order round trip, so folding
                    it into the cancel histogram would distort it. *)
                 let events =
                   Matching_engine.cancel_all_for_participant
                     engine
                     participant
                 in
                 Dispatcher.dispatch dispatcher events;
                 return (Ok ()))
        ; Rpc.Rpc.implement
            Rpc_protocol.reset_exchange_rpc
            (fun (_ : Connection_state.t) () ->
               (* Operator reset: flatten the entire book across every
                  participant, then fan the cancels out to subscribers. No
                  login check — this is a whole-exchange action, not a
                  per-session one. The seed market maker re-quotes on its next
                  tick, so the book refills with baseline liquidity. *)
               let events = Matching_engine.cancel_everything engine in
               Dispatcher.dispatch dispatcher events;
               return (Ok ()))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr _conn ->
        let state = { Connection_state.session = None } in
        don't_wait_for
          (let%bind () = Rpc.Connection.close_finished _conn in
           match state.Connection_state.session with
           | None -> Deferred.unit
           | Some session -> Dispatcher.clean_up_session dispatcher session);
        state)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  (* One global reporting loop, not one per subscriber: it builds a single
     snapshot each second and fans it out. It runs even with no subscribers
     so the latency buckets are always drained, bounding their memory. *)
  let report_stats () =
    let gc = Gc.stat () in
    let book_depth =
      List.filter_map symbols ~f:(fun symbol ->
        Matching_engine.book engine symbol
        |> Option.map ~f:(fun book ->
          { Exchange_stats.Book_depth.symbol
          ; bbo = Order_book.best_bid_offer book
          ; resting_size_bid = Order_book.total_resting_size book Side.Buy
          ; resting_size_ask = Order_book.total_resting_size book Side.Sell
          }))
    in
    (* Merge this interval's per-participant order counts with the current
       resting counts; a participant appears if it did either. *)
    let order_counts =
      Participant.Map.of_alist_exn (Metrics.take_order_counts metrics)
    in
    let resting = Matching_engine.resting_by_participant engine in
    let participants =
      Set.union (Map.key_set order_counts) (Map.key_set resting)
      |> Set.to_list
      |> List.map ~f:(fun participant ->
        { Exchange_stats.Participant_activity.participant
        ; orders_last_interval =
            Map.find order_counts participant |> Option.value ~default:0
        ; resting_orders =
            Map.find resting participant |> Option.value ~default:0
        })
    in
    let max_gap, mean_gap = Metrics.take_engine_gap metrics in
    Metrics.push
      metrics
      { Exchange_stats.live_words = gc.live_words
      ; heap_words = gc.heap_words
      ; top_heap_words = gc.top_heap_words
      ; submit_latency = Metrics.take_submit_histogram metrics
      ; cancel_latency = Metrics.take_cancel_histogram metrics
      ; book_depth
      ; pipe_occupancy = Dispatcher.pipe_occupancy dispatcher
      ; participants
      ; engine =
          { queue_depth = Pipe.length request_writer; max_gap; mean_gap }
      }
  in
  Clock_ns.every
    ~stop:(Tcp.Server.close_finished tcp_server)
    (Time_ns.Span.of_sec 1.)
    report_stats;
  { engine; dispatcher; request_writer; tcp_server; port = actual_port }
;;

let port t = t.port

let close t =
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
