open! Core
open Jsip_types

(* Orders are keyed so that the *best* order on a side — the one that trades
   first under price-time priority — is always [Map.min_elt]:
   - asks: lowest price first, then earliest arrival (lowest order id);
   - bids: highest price first, then earliest arrival (lowest order id).

   Asks get the natural ascending order of [(price, id)]. Bids need
   price *descending* but id *ascending*, and a single tuple comparator can't
   do both, so bids and asks use different key modules. Keeping "best =
   min_elt" on both sides is what makes [find_match] symmetric and correct. *)
module Ask_key = struct
  type t = Price.t * Order_id.t [@@deriving sexp, compare]
end

module Bid_key = struct
  type t = Price.t * Order_id.t [@@deriving sexp]

  let compare (price1, id1) (price2, id2) =
    (* Reverse the price comparison so a higher price ranks first; break ties
       on ascending order id so the earlier arrival ranks first. *)
    match Price.compare price2 price1 with
    | 0 -> Order_id.compare id1 id2
    | not_equal -> not_equal
  ;;
end

module Ask_map = Map.Make (Ask_key)
module Bid_map = Map.Make (Bid_key)

type t =
  { symbol : Symbol.t (* Separate maps for bids and asks *)
  ; mutable bids : Order.t Bid_map.t
  ; mutable asks : Order.t Ask_map.t
  ; (* maps Order_id.t directly to its (Side.t, Price.t) location. *)
    mutable id_index : (Side.t * Price.t) Order_id.Table.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Bid_map.empty
  ; asks = Ask_map.empty
  ; id_index = Order_id.Table.create ()
  }
;;

let symbol t = t.symbol

let add t order =
  let id = Order.order_id order in
  let price = Order.price order in
  let side = Order.side order in
  let key = price, id in
  (* update side maps *)
  match side with
  | Buy ->
    t.bids <- Map.set t.bids ~key ~data:order;
    Hashtbl.set t.id_index ~key:id ~data:(Buy, price)
  | Sell ->
    t.asks <- Map.set t.asks ~key ~data:order;
    Hashtbl.set t.id_index ~key:id ~data:(Sell, price)
;;

let remove' t id =
  match Hashtbl.find t.id_index id with
  | None -> None
  | Some (side, price) ->
    let key = price, id in
    Hashtbl.remove t.id_index id;
    (match side with
     | Buy ->
       let order = Map.find t.bids key in
       t.bids <- Map.remove t.bids key;
       order
     | Sell ->
       let order = Map.find t.asks key in
       t.asks <- Map.remove t.asks key;
       order)
;;

let remove t id = ignore (remove' t id : Order.t option)

let find t order_id =
  (* find the order's map location coordinates *)
  match Hashtbl.find t.id_index order_id with
  | None -> None
  | Some (side, price) ->
    let key = price, order_id in
    (* get the order directly out of the correct map tree *)
    (match side with
     | Buy -> Map.find t.bids key
     | Sell -> Map.find t.asks key)
;;

let find_match t incoming_order =
  let incoming_side = Order.side incoming_order in
  let incoming_price = Order.price incoming_order in
  (* Extract the "best" resting candidate from the opposite side *)
  let best_resting_opt =
    match incoming_side with
    | Buy -> Map.min_elt t.asks (* best ask: lowest price, then earliest *)
    | Sell -> Map.min_elt t.bids (* best bid: highest price, then earliest *)
  in
  match best_resting_opt with
  | None -> None
  | Some ((resting_price, _), resting_order) ->
    (* Verify if the best candidate is actually marketable *)
    if Price.is_marketable incoming_side ~price:incoming_price ~resting_price
    then Some resting_order
    else None
;;

let orders_on_side t side =
  match side with
  | Side.Sell -> Map.data t.asks (* Ascending: Lowest Asks first *)
  | Side.Buy ->
    Map.data
      t.bids (* Bid_map order is best-first: highest price, earliest *)
;;

let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks

let count t side =
  match side with
  | Side.Buy -> Map.length t.bids
  | Side.Sell -> Map.length t.asks
;;

let best_price t side =
  match side with
  | Side.Buy ->
    Map.min_elt t.bids |> Option.map ~f:(fun ((price, _id), _order) -> price)
  | Side.Sell ->
    Map.min_elt t.asks |> Option.map ~f:(fun ((price, _id), _order) -> price)
;;

let best_level t side : Level.t option =
  match best_price t side with
  | None -> None
  | Some price ->
    let orders = orders_on_side t side in
    let total_size =
      List.fold orders ~init:Size.zero ~f:(fun acc order ->
        if Price.equal (Order.price order) price
        then Size.( + ) acc (Order.remaining_size order)
        else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

(* Sort the underlying [Order.t] list with a comparator built from
   [Price.is_more_aggressive] and [Order_id.compare] (lower order id =
   arrived first), then map to [Level.t]. Sorting [Level.t] directly would
   lose the arrival-time tiebreak, since [Level.compare] only knows price and
   size. *)
(* CR claude for dani.rana: this inverts the arrival-time tiebreak.
   [Comparable.reverse] flips the *whole* comparison, so at equal prices the
   base [Order_id.compare order1 order2] (earliest-first) becomes effectively
   [compare order2 order1] (latest-first) — the opposite of price-time
   priority. The snapshot test only uses all-distinct prices, so it never
   exercises the tie and the bug hides. Suggest dropping [reverse] and ranking
   best-price-first with an earliest-id tiebreak directly. Note the [Buy] and
   [Sell] arms are byte-identical, since [Price.is_more_aggressive] already
   folds in [side] — one comparator covers both. *)
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
