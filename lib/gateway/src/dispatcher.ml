open! Core
open! Async
open Jsip_types

(* A bounded, drop-oldest outbound pipe for one market-data subscriber.

   Market-data events are a *state* stream: the newest BBO supersedes the
   last, so when a subscriber can't keep up we want it to keep the freshest
   quotes and lose stale history — not accumulate a backlog it will never
   drain. The producer (the matching loop, via [push]) therefore never
   blocks: it enqueues into a bounded [buffer], dropping the OLDEST event
   once the buffer is full. A per-subscriber [pump] drains [buffer] into the
   RPC-facing pipe with pushback, so a slow reader stalls only its own pump —
   never the shared producer.

   Contrast the session and audit feeds, which *disconnect* a slow consumer
   instead: their events (fills, cancels) are not superseded, so dropping any
   would silently corrupt the client's own view. *)
module Market_data_pipe = struct
  type t =
    { writer : Exchange_event.t Pipe.Writer.t
    ; buffer : Exchange_event.t Queue.t
    ; budget : int
    ; wake : (unit, read_write) Bvar.t
    }

  (* Drain [buffer] into [writer] one event at a time, respecting the RPC
     pipe's pushback. When [buffer] empties, wait for either a [push] (via
     [wake]) or the reader closing, then loop. Stops once the writer is
     closed. *)
  let rec pump t =
    match Queue.dequeue t.buffer with
    | Some event ->
      let%bind () = Pipe.write_if_open t.writer event in
      if Pipe.is_closed t.writer then Deferred.unit else pump t
    | None ->
      if Pipe.is_closed t.writer
      then Deferred.unit
      else (
        let%bind () =
          Deferred.any [ Bvar.wait t.wake; Pipe.closed t.writer ]
        in
        pump t)
  ;;

  let create ~budget =
    let reader, writer = Pipe.create () in
    let t =
      { writer; buffer = Queue.create (); budget; wake = Bvar.create () }
    in
    don't_wait_for (pump t);
    t, reader
  ;;

  (* Never blocks. Enqueue, enforce the bound (drop-oldest), then wake the
     pump. Closed subscribers are ignored — cleanup removes them from the
     bag. *)
  let push t event =
    if not (Pipe.is_closed t.writer)
    then (
      Queue.enqueue t.buffer event;
      (* Enforce the bound by dropping the oldest. We check on every push, so
         the buffer overshoots by at most one; a single dequeue pins it back
         at exactly [budget]. [Queue.dequeue] removes the front — the oldest
         — so the subscriber keeps the freshest quotes and loses stale
         history. *)
      if Queue.length t.buffer > t.budget
      then ignore (Queue.dequeue t.buffer : Exchange_event.t option);
      Bvar.broadcast t.wake ())
  ;;

  (* Events buffered but not yet handed to the reader — the slow-consumer
     signal, now bounded by [budget]. *)
  let length t = Queue.length t.buffer
  let closed t = Pipe.closed t.writer
end

(* Default per-family pipe budgets, used when [create] is not given explicit
   ones. Market data is smaller: it is a keep-the-latest state stream, so a
   shallow buffer bounds staleness; session/audit are event streams a client
   must not miss, so they get more slack before we disconnect. *)
let default_market_data_budget = 256
let default_session_budget = 1024
let default_audit_budget = 1024

type t =
  { market_data_subscribers_by_symbol :
      Market_data_pipe.t Bag.t Symbol.Table.t
  ; audit_subscribers : Exchange_event.t Pipe.Writer.t Bag.t
  ; sessions_table : Session.t Participant.Table.t
  ; market_data_budget : int
  ; session_budget : int
  ; audit_budget : int
  }

let create
  ?(market_data_budget = default_market_data_budget)
  ?(session_budget = default_session_budget)
  ?(audit_budget = default_audit_budget)
  ()
  =
  { market_data_subscribers_by_symbol = Symbol.Table.create ()
  ; audit_subscribers = Bag.create ()
  ; sessions_table = Participant.Table.create ()
  ; market_data_budget
  ; session_budget
  ; audit_budget
  }
;;

(* Build a session with this dispatcher's configured [session_budget], so
   every session shares one disconnect threshold. When the session's pipe
   closes — including a slow-consumer disconnect in [Session.push] — drop it
   from the registry so the participant can reconnect, but only if it is
   still the registered session: a re-login may have replaced it, and we must
   not evict the replacement. *)
let create_session t participant =
  let session = Session.create ~budget:t.session_budget participant in
  don't_wait_for
    (let%map () = Session.closed session in
     match Hashtbl.find t.sessions_table participant with
     | Some current when phys_equal current session ->
       Hashtbl.remove t.sessions_table participant
     | Some _ | None -> ());
  session
;;

let sessions t = t.sessions_table

let clean_up_session t session =
  let table = sessions t in
  let participant = Session.participant session in
  Hashtbl.remove table participant;
  Session.close session;
  Deferred.return ()
;;

let set_up_session t participant =
  let table = sessions t in
  let%bind () =
    match Hashtbl.find table participant with
    | Some _session -> clean_up_session t _session
    | None -> Deferred.return ()
  in
  let new_session = create_session t participant in
  Hashtbl.set table ~key:participant ~data:new_session;
  Deferred.return ()
;;

let subscribe_market_data t symbols =
  let handle, reader =
    Market_data_pipe.create ~budget:t.market_data_budget
  in
  (* Register the same handle in every requested symbol's bag. A per-symbol
     publish iterates a single bag, so a subscriber listed in multiple bags
     receives each event exactly once — only via whichever bag matches the
     event's symbol. *)
  let elts =
    List.map symbols ~f:(fun symbol ->
      let subscribers =
        Hashtbl.find_or_add
          t.market_data_subscribers_by_symbol
          ~default:Bag.create
          symbol
      in
      symbol, Bag.add subscribers handle)
  in
  don't_wait_for
    (let%map () = Market_data_pipe.closed handle in
     List.iter elts ~f:(fun (symbol, elt) ->
       match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
       | None -> ()
       | Some subscribers -> Bag.remove subscribers elt));
  reader
;;

let subscribe_audit t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.audit_subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.audit_subscribers elt);
  reader
;;

let push_market_data t event symbol =
  match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
  | None -> ()
  | Some subscribers ->
    Bag.iter subscribers ~f:(fun handle ->
      Market_data_pipe.push handle event)
;;

(* Audit is an event firehose (every event, in order), not a state stream, so
   dropping records would corrupt the operator's view. When a subscriber
   falls a full [audit_budget] behind we disconnect it instead: close its
   pipe (its [Pipe.closed] cleanup removes it from the bag) and log. A
   monitor that can't keep up reconnects and resyncs rather than silently
   missing events. *)
let push_audit t event =
  let budget = t.audit_budget in
  Bag.iter t.audit_subscribers ~f:(fun writer ->
    if Pipe.is_closed writer
    then ()
    else if Pipe.length writer >= budget
    then (
      [%log.error "audit subscriber too slow; disconnecting" (budget : int)];
      Pipe.close writer)
    else Pipe.write_without_pushback_if_open writer event)
;;

(* writes the event to the appropriate session's pipe. *)
let push_to_session t participant event =
  let table = sessions t in
  match Hashtbl.find table participant with
  | Some session -> Session.push session event
  | None -> ()
;;

let dispatch_event t (event : Exchange_event.t) =
  push_audit t event;
  match event with
  | Best_bid_offer_update { symbol; bbo = _ } ->
    push_market_data t event symbol
  | Trade_report { symbol; price = _; size = _ } ->
    push_market_data t event symbol
  | Order_accept { order_id = _; request }
  | Order_reject { request; reason = _ } ->
    push_to_session t request.participant event
  | Order_cancel
      { order_id = _
      ; participant
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      ; client_order_id = _
      } ->
    push_to_session t participant event
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size = _
      ; aggressor_order_id = _
      ; aggressor_participant
      ; aggressor_side = _
      ; aggressor_client_order_id = _
      ; resting_order_id = _
      ; resting_participant
      ; resting_client_order_id = _
      } ->
    push_to_session t aggressor_participant event;
    push_to_session t resting_participant event
  | Cancel_reject { participant; _ } -> push_to_session t participant event
;;

let dispatch t events = List.iter events ~f:(dispatch_event t)

(* Snapshot the worst queue depth of each family of outbound pipe.
   [Pipe.length] is the count of elements buffered but not yet read; a slow
   consumer is the one whose length climbs while the others stay near zero. *)
let pipe_occupancy t : Exchange_stats.Pipe_occupancy.t =
  (* Market-data occupancy is the drop-oldest buffer's depth (bounded by its
     budget); audit occupancy is the raw pipe length. *)
  let market_data_max =
    Hashtbl.fold
      t.market_data_subscribers_by_symbol
      ~init:0
      ~f:(fun ~key:_ ~data:bag acc ->
        Bag.fold bag ~init:acc ~f:(fun acc handle ->
          Int.max acc (Market_data_pipe.length handle)))
  in
  let audit_max =
    Bag.fold t.audit_subscribers ~init:0 ~f:(fun acc writer ->
      Int.max acc (Pipe.length writer))
  in
  let session_max, slowest_session =
    Hashtbl.fold
      t.sessions_table
      ~init:(0, None)
      ~f:(fun ~key:participant ~data:session (worst, who) ->
        let length = Session.queue_length session in
        if length > worst then length, Some participant else worst, who)
  in
  { market_data_max; audit_max; session_max; slowest_session }
;;

module For_testing = struct
  let audit_subscriber_count t = Bag.length t.audit_subscribers
end
