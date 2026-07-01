open! Core
open! Async
open Jsip_types
(* open Jsip_gateway *)

module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; client_order_id : Client_order_id.t
    ; inventory_skew_cents_per_share : int
    }
  [@@deriving sexp_of]
end

(* let update_inventory inventory (fill : Fill.t) ~is_aggressor ~fill_size =
  let current_inv =
    Option.value (Hashtbl.find inventory fill.symbol) ~default:0
  in
  let new_inventory =
    match fill.aggressor_side with
    | Buy ->
      if is_aggressor
      then current_inv + fill_size
      else current_inv - fill_size
    | Sell ->
      if is_aggressor
      then current_inv - fill_size
      else current_inv + fill_size
  in
  Hashtbl.set inventory ~key:fill.symbol ~data:new_inventory;
  new_inventory
;;

let update_active_orders active_orders ~my_client_order_id ~fill_size =
  match Hashtbl.find active_orders my_client_order_id with
  | None -> ()
  | Some remaining_size ->
    let new_size = remaining_size - fill_size in
    if new_size <= 0
    then Hashtbl.remove active_orders my_client_order_id
    else Hashtbl.set active_orders ~key:my_client_order_id ~data:new_size
;;

let seed_book (config : Config.t) conn ~fair_value_cents =
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
    in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  let base_id = Client_order_id.to_int config.client_order_id in
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let buy_id = Client_order_id.of_int (base_id + (level * 2)) in
      let sell_id = Client_order_id.of_int (base_id + (level * 2) + 1) in
      let%bind () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Buy
           ; price = Price.of_int_cents (fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = buy_id
           }
           : Order.Request.t)
      and () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = sell_id
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

let cancel_and_re_quote (config : Config.t) conn active_orders ~new_inventory
  =
  let orders_to_cancel = Hashtbl.keys active_orders in
  don't_wait_for
    (let%bind () =
       Deferred.List.iter orders_to_cancel ~how:`Parallel ~f:(fun order_id ->
         let%map result =
           Rpc.Rpc.dispatch Rpc_protocol.cancel_order_rpc conn order_id
         in
         result |> Or_error.ok_exn |> Or_error.ignore_m |> Or_error.ok_exn)
     in
     let skewed_fair =
       config.fair_value_cents
       - (new_inventory * config.inventory_skew_cents_per_share)
     in
     seed_book config conn ~fair_value_cents:skewed_fair)
;;

let run (config : Config.t) conn =
  (* inventory counter per symbol *)
  let inventory = Symbol.Table.create () in
  (* set of currently resting client order ids *)
  let active_orders = Client_order_id.Table.create () in
  let%bind () =
    seed_book config conn ~fair_value_cents:config.fair_value_cents
  in
  (* subscribe to the session feed protocol *)
  let%bind pipe_reader, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  let%bind () =
    Pipe.iter_without_pushback pipe_reader ~f:(fun event ->
      match event with
      (* record id from each accept *)
      | Order_accept accept ->
        if Participant.equal accept.request.participant config.participant
        then (
          let size = Size.to_int accept.request.size in
          Hashtbl.set
            active_orders
            ~key:accept.request.client_order_id
            ~data:size
          (* remove id on cancel *))
      | Order_cancel cancel ->
        if Participant.equal cancel.participant config.participant
        then Hashtbl.remove active_orders cancel.client_order_id
      | Fill fill ->
        let fill_size = Size.to_int fill.size in
        let is_aggressor =
          Participant.equal fill.aggressor_participant config.participant
        in
        let new_inventory =
          update_inventory inventory fill ~is_aggressor ~fill_size
        in
        (* get client order ID *)
        let my_client_order_id =
          if is_aggressor
          then fill.aggressor_client_order_id
          else fill.resting_participant_client_order_id
        in
        update_active_orders active_orders ~my_client_order_id ~fill_size;
        cancel_and_re_quote config conn active_orders ~new_inventory
      | Order_reject _ | Best_bid_offer_update _ | Trade_report _
      | Cancel_reject _ ->
        ())
  in
  Deferred.never ()
;; *)
