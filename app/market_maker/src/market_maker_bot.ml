open! Core
open! Async
open Jsip_types
module Bot_runtime = Jsip_bot_runtime.Bot_runtime
module Context = Bot_runtime.Context

module Market_maker_bot = struct
  module Config = struct
    type t =
      { symbol : Symbol.t
      ; fair_value_cents : int
      ; half_spread_cents : int
      ; size_per_level : int
      ; num_levels : int
      ; client_order_id : Client_order_id.t
      ; inventory_skew_cents_per_share : int
      }
    [@@deriving sexp_of]
  end

  let name = "market_maker"

  (* a state for a single bot *)
  module Bot_state = struct
    type t =
      { inventory : int Symbol.Table.t
      ; active_orders : int Client_order_id.Table.t
      }

    let create () =
      { inventory = Symbol.Table.create ()
      ; active_orders = Client_order_id.Table.create ()
      }
    ;;
  end

  let seed_book
    (state : Bot_state.t)
    (config : Config.t)
    (context : Context.t)
    ~fair_value_cents
    =
    Hashtbl.set
      state.active_orders
      ~key:config.client_order_id
      ~data:config.size_per_level;
    let submit request =
      let%map result = Context.submit context request in
      match result with
      | Ok () -> ()
      | Error msg ->
        Hashtbl.remove state.active_orders config.client_order_id;
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
             ; participant = Context.participant context
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
             ; participant = Context.participant context
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

  let update_inventory
    (state : Bot_state.t)
    (fill : Fill.t)
    ~is_aggressor
    ~fill_size
    =
    let current_inv =
      Option.value (Hashtbl.find state.inventory fill.symbol) ~default:0
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
    Hashtbl.set state.inventory ~key:fill.symbol ~data:new_inventory;
    new_inventory
  ;;

  let update_active_orders
    (state : Bot_state.t)
    ~my_client_order_id
    ~fill_size
    =
    match Hashtbl.find state.active_orders my_client_order_id with
    | None -> ()
    | Some remaining_size ->
      let new_size = remaining_size - fill_size in
      if new_size <= 0
      then Hashtbl.remove state.active_orders my_client_order_id
      else
        Hashtbl.set
          state.active_orders
          ~key:my_client_order_id
          ~data:new_size
  ;;

  let cancel_and_re_quote
    (state : Bot_state.t)
    (config : Config.t)
    context
    ~new_inventory
    =
    let orders_to_cancel = Hashtbl.keys state.active_orders in
    don't_wait_for
      (let%bind () =
         Deferred.List.iter
           orders_to_cancel
           ~how:`Parallel
           ~f:(fun client_order_id ->
             let%map result = Context.cancel context client_order_id in
             match result with
             | Ok () -> ()
             | Error err ->
               [%log.error
                 "market_maker: cancel failed"
                   (client_order_id : Client_order_id.t)
                   (err : Error.t)])
       in
       let skewed_fair =
         config.fair_value_cents
         - (new_inventory * config.inventory_skew_cents_per_share)
       in
       seed_book state config context ~fair_value_cents:skewed_fair)
  ;;

  (* initlizating *)
  let on_start
    (state : Bot_state.t)
    (config : Config.t)
    (context : Context.t)
    =
    seed_book state config context ~fair_value_cents:config.fair_value_cents
  ;;

  let on_tick (_config : Config.t) (_context : Context.t) = Deferred.unit

  (* the bot handles every event that it is involved in *)
  let on_event
    (config : Config.t)
    (state : Bot_state.t)
    (context : Context.t)
    (event : Exchange_event.t)
    =
    let my_participant = Context.participant context in
    match event with
    | Order_accept accept ->
      if Participant.equal accept.request.participant my_participant
      then (
        let size = Size.to_int accept.request.size in
        Hashtbl.set
          state.active_orders
          ~key:accept.request.client_order_id
          ~data:size);
      Deferred.unit
    | Order_cancel cancel ->
      if Participant.equal cancel.participant my_participant
      then Hashtbl.remove state.active_orders cancel.client_order_id;
      Deferred.unit
    | Fill fill ->
      let is_aggressor =
        Participant.equal fill.aggressor_participant my_participant
      in
      let fill_size = Size.to_int fill.size in
      let new_inventory =
        update_inventory state fill ~is_aggressor ~fill_size
      in
      let my_client_order_id =
        if is_aggressor
        then fill.aggressor_client_order_id
        else fill.resting_participant_client_order_id
      in
      update_active_orders state ~my_client_order_id ~fill_size;
      cancel_and_re_quote state config context ~new_inventory;
      Deferred.unit
    | Order_reject _ | Best_bid_offer_update _ | Trade_report _
    | Cancel_reject _ ->
      Deferred.unit
  ;;
end
