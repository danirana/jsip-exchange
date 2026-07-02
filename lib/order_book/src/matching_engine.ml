open! Core
open Jsip_types

type t =
  { books : Order_book.t Symbol.Map.t
  ; order_id_gen : Order_id.Generator.t
  ; mutable next_fill_id : int
  ; seen_client_order_ids : Order.t Option.t Int.Table.t Participant.Table.t
  }
[@@deriving sexp_of]

let create symbols =
  let books =
    List.map symbols ~f:(fun sym -> sym, Order_book.create sym)
    |> Symbol.Map.of_alist_exn
  in
  { books
  ; order_id_gen = Order_id.Generator.create ()
  ; next_fill_id = 1
  ; seen_client_order_ids = Participant.Table.create ()
  }
;;

let book t symbol = Map.find t.books symbol

let mark_id_as_terminal t participant client_order_id =
  match Hashtbl.find t.seen_client_order_ids participant with
  | None -> ()
  | Some id_table ->
    let raw_cl_id = Client_order_id.to_int client_order_id in
    if Hashtbl.mem id_table raw_cl_id
    then Hashtbl.set id_table ~key:raw_cl_id ~data:None
;;

(** Run the matching loop: repeatedly find a compatible resting order and
    fill against it. Returns the list of Fill and Trade_report events
    produced, and the next fill_id to use. *)
let rec match_loop ~engine ~book ~order ~fill_id =
  if Size.( <= ) (Order.remaining_size order) Size.zero
  then [], fill_id
  else (
    match Order_book.find_match book order with
    | None -> [], fill_id
    | Some resting ->
      let fill_size =
        Size.min (Order.remaining_size order) (Order.remaining_size resting)
      in
      Order.fill order ~by:fill_size;
      Order.fill resting ~by:fill_size;
      if Order.is_fully_filled resting
      then (
        Order_book.remove book (Order.order_id resting);
        mark_id_as_terminal
          engine
          (Order.participant resting)
          (Order.client_order_id resting));
      let fill_event =
        Exchange_event.Fill
          { fill_id
          ; symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          ; aggressor_order_id = Order.order_id order
          ; aggressor_participant = Order.participant order
          ; aggressor_side = Order.side order
          ; aggressor_client_order_id = Order.client_order_id order
          ; resting_order_id = Order.order_id resting
          ; resting_participant = Order.participant resting
          ; resting_client_order_id =
              Order.client_order_id resting
          }
      in
      let trade_event =
        Exchange_event.Trade_report
          { symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          }
      in
      let remaining_events, next_fill_id =
        match_loop ~engine ~book ~order ~fill_id:(fill_id + 1)
      in
      fill_event :: trade_event :: remaining_events, next_fill_id)
;;

let submit t (request : Order.Request.t) =
  let id_table =
    Hashtbl.find_or_add
      t.seen_client_order_ids
      request.participant
      ~default:Int.Table.create
  in
  let raw_cl_id = Client_order_id.to_int request.client_order_id in
  (* If ID key exists it is a duplicate *)
  if Hashtbl.mem id_table raw_cl_id
  then
    [ Exchange_event.Order_reject
        { request; reason = "duplicate client_order_id" }
    ]
  else (
    match Map.find t.books request.symbol with
    | None ->
      [ Exchange_event.Order_reject { request; reason = "unknown symbol" } ]
    | Some book ->
      let order_id = Order_id.Generator.next t.order_id_gen in
      let order = Order.create request ~order_id in
      let accepted = Exchange_event.Order_accept { order_id; request } in
      (* Track it *)
      Hashtbl.set id_table ~key:raw_cl_id ~data:(Some order);
      (* Snapshot BBO before matching so we can detect changes. *)
      let bbo_before = Order_book.best_bid_offer book in
      (* Match *)
      let fill_events, next_fill_id =
        match_loop ~engine:t ~book ~order ~fill_id:t.next_fill_id
      in
      t.next_fill_id <- next_fill_id;
      (* Post-match: rest on book or cancel unfilled remainder. *)
      let post_events =
        if Size.( > ) (Order.remaining_size order) Size.zero
        then (
          match Order.time_in_force order with
          | Day ->
            Order_book.add book order;
            []
          | Ioc ->
            mark_id_as_terminal t request.participant request.client_order_id;
            [ Exchange_event.Order_cancel
                { order_id
                ; participant = Order.participant order
                ; symbol = Order.symbol order
                ; remaining_size = Order.remaining_size order
                ; reason = Ioc_remainder
                ; client_order_id = Order.client_order_id order
                }
            ])
        else (
          mark_id_as_terminal t request.participant request.client_order_id;
          [])
      in
      (* Emit BBO update if the best bid or ask changed. *)
      let bbo_after = Order_book.best_bid_offer book in
      let bbo_events =
        if Bbo.equal bbo_before bbo_after
        then []
        else
          [ Exchange_event.Best_bid_offer_update
              { symbol = Order.symbol order; bbo = bbo_after }
          ]
      in
      List.concat [ [ accepted ]; fill_events; post_events; bbo_events ])
;;

let cancel t participant client_order_id =
  let raw_cl_id = Client_order_id.to_int client_order_id in
  let active_order_opt =
    match Hashtbl.find t.seen_client_order_ids participant with
    | None -> None
    | Some id_table ->
      (match Hashtbl.find id_table raw_cl_id with
       | None | Some None -> None
       | Some (Some order) -> Some order)
  in
  match active_order_opt with
  | None ->
    [ Exchange_event.Cancel_reject
        { participant; client_order_id; reason = "order not found" }
    ]
  | Some order ->
    let symbol = Order.symbol order in
    (match Map.find t.books symbol with
     | None ->
       [ Exchange_event.Cancel_reject
           { participant; client_order_id; reason = "unknown symbol" }
       ]
     | Some book ->
       let bbo_before = Order_book.best_bid_offer book in
       (* shift to a dead terminal state *)
       Order_book.remove book (Order.order_id order);
       mark_id_as_terminal t participant client_order_id;
       let cancel_event =
         Exchange_event.Order_cancel
           { order_id = Order.order_id order
           ; participant
           ; symbol
           ; remaining_size = Order.remaining_size order
           ; reason = Participant_requested
           ; client_order_id
           }
       in
       let bbo_after = Order_book.best_bid_offer book in
       let bbo_events =
         if Bbo.equal bbo_before bbo_after
         then []
         else
           [ Exchange_event.Best_bid_offer_update { symbol; bbo = bbo_after }
           ]
       in
       cancel_event :: bbo_events)
;;
