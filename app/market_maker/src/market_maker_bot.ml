open! Core
open! Async
open Jsip_types
module Bot_runtime = Jsip_bot_runtime.Bot_runtime
module Context = Bot_runtime.Context

(* A dynamic market-making {!Bot_runtime.Bot}.

   The bot quotes a symmetric ladder of resting bids and asks around a
   configured fair value, then re-centers on every fill: it tracks per-symbol
   inventory and skews its fair value against that inventory (quoting lower
   when long, higher when short) so it naturally trades back toward flat.

   Per-bot mutable state — inventory and the set of live client order ids —
   lives in [Config.t]. The [Bot_runtime.Bot] callbacks receive only the
   config and the context, so the config is the one place all three callbacks
   can share state. *)

module Config = struct
  type t =
    { symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; mutable client_order_id : Client_order_id.t
    (** Base id for the next ladder; [seed_book] advances it by one full ladder
        each call, so every re-quote uses fresh ids that the exchange won't
        reject as duplicates of already-terminal ones. *)
    ; inventory_skew_cents_per_share : int
    ; inventory : int Symbol.Table.t
    ; active_orders : int Client_order_id.Table.t
    }
  [@@deriving sexp_of]

  let create
    ~symbol
    ~fair_value_cents
    ~half_spread_cents
    ~size_per_level
    ~num_levels
    ~client_order_id
    ~inventory_skew_cents_per_share
    =
    { symbol
    ; fair_value_cents
    ; half_spread_cents
    ; size_per_level
    ; num_levels
    ; client_order_id
    ; inventory_skew_cents_per_share
    ; inventory = Symbol.Table.create ()
    ; active_orders = Client_order_id.Table.create ()
    }
  ;;
end

let name = "market_maker"

(* Place [num_levels] resting bids and asks, symmetric around
   [fair_value_cents]. Each level's client order id is derived from the base
   [config.client_order_id] so ids stay unique across levels and sides. The
   live-order table is populated from [Order_accept] events in [on_event],
   not here — a submit that fails in transit never rests, so there is nothing
   to track. *)
let seed_book (config : Config.t) (context : Context.t) ~fair_value_cents =
  let submit request =
    let%map result = Context.submit context request in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  let base_id = Client_order_id.to_int config.client_order_id in
  (* Advance the base by one whole ladder (two ids per level) so the next
     re-quote — after a fill, or after the book is reset out from under us —
     mints ids the exchange hasn't seen, instead of colliding with the ones we
     just cancelled. *)
  config.client_order_id
  <- Client_order_id.of_int (base_id + (config.num_levels * 2));
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

(* Update per-symbol inventory for a fill involving this bot. A buy adds to
   inventory and a sell subtracts; whether the bot was the aggressor or the
   resting side flips which direction [aggressor_side] means for us. *)
let update_inventory
  (config : Config.t)
  (fill : Fill.t)
  ~is_aggressor
  ~fill_size
  =
  let current_inv =
    Option.value (Hashtbl.find config.inventory fill.symbol) ~default:0
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
  Hashtbl.set config.inventory ~key:fill.symbol ~data:new_inventory;
  new_inventory
;;

let update_active_orders (config : Config.t) ~my_client_order_id ~fill_size =
  match Hashtbl.find config.active_orders my_client_order_id with
  | None -> ()
  | Some remaining_size ->
    let new_size = remaining_size - fill_size in
    if new_size <= 0
    then Hashtbl.remove config.active_orders my_client_order_id
    else
      Hashtbl.set config.active_orders ~key:my_client_order_id ~data:new_size
;;

(* Cancel the whole live ladder and re-seed it around a fair value skewed
   against current inventory, so the bot leans toward unwinding its position. *)
let cancel_and_re_quote (config : Config.t) context ~new_inventory =
  let orders_to_cancel = Hashtbl.keys config.active_orders in
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
     seed_book config context ~fair_value_cents:skewed_fair)
;;

let on_start (config : Config.t) (context : Context.t) =
  seed_book config context ~fair_value_cents:config.fair_value_cents
;;

(* Keep the ladder alive. Fills re-quote via [cancel_and_re_quote]; this
   handles the ladder being cancelled out from under us — e.g. an operator
   exchange reset — by re-seeding around fair value whenever we hold no live
   orders. [seed_book] advances the id base, so the re-seed is accepted. *)
let on_tick (config : Config.t) (context : Context.t) =
  if Hashtbl.is_empty config.active_orders
  then seed_book config context ~fair_value_cents:config.fair_value_cents
  else Deferred.unit
;;

let on_event
  (config : Config.t)
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
        config.active_orders
        ~key:accept.request.client_order_id
        ~data:size);
    Deferred.unit
  | Order_cancel cancel ->
    if Participant.equal cancel.participant my_participant
    then Hashtbl.remove config.active_orders cancel.client_order_id;
    Deferred.unit
  | Fill fill ->
    let is_aggressor =
      Participant.equal fill.aggressor_participant my_participant
    in
    let fill_size = Size.to_int fill.size in
    let new_inventory =
      update_inventory config fill ~is_aggressor ~fill_size
    in
    let my_client_order_id =
      if is_aggressor
      then fill.aggressor_client_order_id
      else fill.resting_client_order_id
    in
    update_active_orders config ~my_client_order_id ~fill_size;
    cancel_and_re_quote config context ~new_inventory;
    Deferred.unit
  | Order_reject _ | Best_bid_offer_update _ | Trade_report _
  | Cancel_reject _ ->
    Deferred.unit
;;
