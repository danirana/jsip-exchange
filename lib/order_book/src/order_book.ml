open! Core
open Jsip_types
(* open Async_log_kernel.Ppx_log_syntax *)

 (* type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t list
  ; mutable asks : Order.t list
  }
[@@deriving sexp_of] *)


module Order_key = struct
  type t = Price.t * Order_id.t [@@deriving sexp, compare]
end

module Order_map = Map.Make (Order_key)

type t =
  { symbol : Symbol.t
  (* Separate maps for bids and asks *)
  ; mutable bids : Order.t Order_map.t
  ; mutable asks : Order.t Order_map.t
  ; (* maps Order_id.t directly to its (Side.t, Price.t) location. *)
    mutable id_index : (Side.t * Price.t) Order_id.Table.t
  }
  [@@deriving sexp_of]

let create symbol = { symbol; bids = Order_map.empty; asks = Order_map.empty; id_index = Order_id.Table.create () }  ;;
let symbol t = t.symbol ;;

(*
let side_list t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_list t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;
*)

let add t order =
  let id = Order.order_id order in
  let price = Order.price order in
  let side = Order.side order in
  let key = (price, id) in
  
  (* update side maps *)
  match side with
  | Buy -> 
      t.bids <- Map.set t.bids ~key ~data:order;
      Hashtbl.set t.id_index ~key:id ~data:(Buy, price)
  | Sell -> 
      t.asks <- Map.set t.asks ~key ~data:order;
      Hashtbl.set t.id_index ~key:id ~data:(Sell, price)
;;

(*
OLD ADD FUNCTION
let add t order =
  let side = Order.side order in
  set_side_list t side (order :: side_list t side)
;; *)

(* 
OLD REMOVE FUNCTION
let remove' t order_id =
  let remove_from t side order_id =
    let orders = side_list t side in
    match
      List.partition_tf orders ~f:(fun o ->
        Order_id.equal (Order.order_id o) order_id)
    with
    | [], _ -> None
    | [ found ], rest ->
      set_side_list t side rest;
      Some found
    | matches, _ ->
      [%log.info
        "BUG: More than one order matching order_id found when removing"
          (order_id : Order_id.t)
          (matches : Order.t list)
          (t.symbol : Symbol.t)
          (side : Side.t)];
      None
  in
  match remove_from t Buy order_id with
  | Some _ as result -> result
  | None -> remove_from t Sell order_id
;;
*)

let remove' t id = 
  match Hashtbl.find t.id_index id with
  | None -> None 
  | Some (side, price) -> 
      let key = (price, id) in
      Hashtbl.remove t.id_index id;
      match side with
      | Buy -> 
          let order = Map.find t.bids key in
          t.bids <- Map.remove t.bids key;
          order
      | Sell -> 
          let order = Map.find t.asks key in
          t.asks <- Map.remove t.asks key;
          order
;;

let remove t id = 
  ignore (remove' t id : Order.t option)
;;
(*
let remove t order_id = ignore (remove' t order_id : Order.t option)
*)

(*
OLD FIND FUNCTION
let find t order_id =
  let find_in side =
    List.find (side_list t side) ~f:(fun o ->
      Order_id.equal (Order.order_id o) order_id)
  in
  match find_in Buy with Some _ as result -> result | None -> find_in Sell
;;
*)

let find t order_id =
  (* find the order's map location coordinates *)
  match Hashtbl.find t.id_index order_id with
  | None -> None 
  | Some (side, price) ->
      let key = (price, order_id) in
      let target_map = 
        match side with
        | Buy  -> t.bids
        | Sell -> t.asks
      in
      (* get the order directly out of the map tree *)
      Map.find target_map key
;;


(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)
(* let find_match t incoming = let incoming_side = Order.side incoming in let
   opposite_side = Side.flip incoming_side in let resting_orders = side_list
   t opposite_side in let is_marketable ~price ~resting_price = match
   (incoming_side : Side.t) with | Buy -> Price.( >= ) price resting_price |
   Sell -> Price.( <= ) price resting_price in List.find resting_orders
   ~f:(fun resting -> is_marketable ~price:(Order.price incoming)
   ~resting_price:(Order.price resting)) ;; *)

let find_match t incoming_order =
  let incoming_side = Order.side incoming_order in
  let incoming_price = Order.price incoming_order in
  (* Extract the "best" resting candidate from the opposite side *)
  let best_resting_opt =
    match incoming_side with
    | Buy -> Map.min_elt t.asks (* Lowest ask wins *)
    | Sell -> Map.max_elt t.bids (* Highest bid wins *)
  in
  match best_resting_opt with
  | None -> None
  | Some ((resting_price, _), resting_order) ->
    (* Verify if the best candidate is actually marketable *)
    if Price.is_marketable incoming_side ~price:incoming_price ~resting_price
    then Some resting_order
    else None
;;

(*
OLD FIND MATCH
let find_match t incoming_order =
  let incoming_side = Order.side incoming_order in
  let opposite_side = Side.flip incoming_side in
  let resting_orders = side_list t opposite_side in
  List.filter resting_orders ~f:(fun resting_order ->
    Price.is_marketable
      incoming_side
      ~price:(Order.price incoming_order)
      ~resting_price:(Order.price resting_order))
  |> List.sort ~compare:(fun order1 order2 ->
    let price_cmp =
      Price.compare (Order.price order1) (Order.price order2)
    in
    if price_cmp <> 0
    then (
      match incoming_side with Buy -> price_cmp | Sell -> -1 * price_cmp)
    else Order_id.compare (Order.order_id order1) (Order.order_id order2))
  |> List.hd
;;
*)

let orders_on_side t side =
  match side with
  | Side.Sell -> 
      Map.data t.asks  (* Ascending: Lowest Asks first *)
  | Side.Buy -> 
      Map.data t.bids |> List.rev (* Descending: Highest Bids first *)
;;

(* 
OLD ORDERS_ON_SIDE FUNCTION
let orders_on_side t side = side_list t side *)


let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks ;;

let count t side = 
  match side with
  | Side.Buy -> Map.length t.bids
  | Side.Sell -> Map.length t.asks
;;

(* 
OLD BEST PRICE FUNCTION
let best_price t side =
  let prices = List.map (side_list t side) ~f:Order.price in
  List.reduce prices ~f:(fun price1 price2 ->
    if Price.is_more_aggressive side ~price:price1 ~than:price2
    then price1
    else price2)
;; *)

let best_price t side = 
  match side with
  | Side.Buy -> 
      Map.max_elt t.bids |> Option.map ~f:(fun ((price, _id), _order) -> price)
  | Side.Sell -> 
      Map.min_elt t.asks |> Option.map ~f:(fun ((price, _id), _order) -> price)
;;

(* match side_list t side with | [] -> None | first :: rest -> let is_better
   = match (side : Side.t) with Buy -> Price.( > ) | Sell -> Price.( < ) in
   Some (List.fold rest ~init:(Order.price first) ~f:(fun best order -> let
   price = Order.price order in if is_better price best then price else
   best))
*)




let best_level t side : Level.t option = 
  match best_price t side with
  | None -> None
  | Some price -> 
      let orders = orders_on_side t side in
      let total_size = List.fold orders ~init:Size.zero ~f:(fun acc order -> 
        if Price.equal (Order.price order) price 
        then Size.( + ) acc (Order.remaining_size order) 
        else acc
      ) in
      Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

(* For snapshot_side: the current code maps orders to Level.t
   ([{ price; size }]) and sorts them with Level.compare, which only knows
   about price and size — it has no notion of arrival time. Sort the
   underlying Order.t list first, with a comparator built from
   Price.is_more_aggressive and Order_id.compare (lower order ID = arrived
   first), then map to Level.t. That keeps the snapshot consistent with the
   matching order. *)

let snapshot_side t (side : Side.t) =
  let compare =
    match side with
    | Buy ->
      Comparable.reverse (fun order1 order2 ->
        let order1_price = Order.price order1 in
        let order2_price = Order.price order2 in
        if Price.equal order1_price order2_price
        then Order_id.compare (Order.order_id order1) (Order.order_id order2)
        else if Price.is_more_aggressive
                  side
                  ~price:order1_price
                  ~than:order2_price
        then 1
        else -1)
    | Sell ->
      Comparable.reverse (fun order1 order2 ->
        let order1_price = Order.price order1 in
        let order2_price = Order.price order2 in
        if Price.equal order1_price order2_price
        then Order_id.compare (Order.order_id order1) (Order.order_id order2)
        else if Price.is_more_aggressive
                  side
                  ~price:order1_price
                  ~than:order2_price
        then 1
        else -1)
  in
  orders_on_side t side |> List.sort ~compare |> List.map ~f:Level.of_order
;;

(* List.filter resting_orders ~f:(fun resting_order ->
   Price.is_more_aggressive incoming_side ~price:(Order.price resting_order)
   ~resting_price:(Order.price incoming_order)) *)
(* let snapshot_side t (side : Side.t) = let compare = match side with | Buy
   -> Comparable.reverse Level.compare | Sell -> Level.compare in
   orders_on_side t side |> List.map ~f:Level.of_order |> List.sort ~compare
   ;; *)

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end
