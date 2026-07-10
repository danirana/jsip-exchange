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
  { symbol : Symbol_id.t (* Separate maps for bids and asks *)
  ; mutable bids : Order.t Bid_map.t
  ; mutable asks : Order.t Ask_map.t
  ; (* maps Order_id.t directly to its (Side.t, Price.t) location. *)
    mutable id_index : (Side.t * Price.t) Order_id.Table.t
  ; (* live count of resting orders per participant, maintained in [add] and
       [remove'] — the only two places the book gains or loses an order. Kept
       here so the count is O(1) to update instead of an O(book) scan. *)
    resting_counts : (int Participant.Table.t[@sexp.opaque])
  ; (* Running sum of [remaining_size] over all resting orders on each side,
       kept O(1) so a once-per-second dashboard poll doesn't rescan the whole
       book (which grows without bound under a book-filler). A side's total
       changes in exactly three places: [add] (+ the order's remaining),
       [remove'] (- the removed order's remaining), and [record_resting_fill]
       (- a partial fill the matching engine applies to a resting order in
       place). Invariant: [total_size_<side>] equals [total_resting_size] a
       full scan would compute. *)
    mutable total_size_bid : Size.t
  ; mutable total_size_ask : Size.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Bid_map.empty
  ; asks = Ask_map.empty
  ; id_index = Order_id.Table.create ()
  ; resting_counts = Participant.Table.create ()
  ; total_size_bid = Size.zero
  ; total_size_ask = Size.zero
  }
;;

let symbol t = t.symbol

let add t order =
  let id = Order.order_id order in
  let price = Order.price order in
  let side = Order.side order in
  let key = price, id in
  Hashtbl.incr t.resting_counts (Order.participant order);
  (* update side maps *)
  let size = Order.remaining_size order in
  match side with
  | Buy ->
    t.bids <- Map.set t.bids ~key ~data:order;
    Hashtbl.set t.id_index ~key:id ~data:(Buy, price);
    t.total_size_bid <- Size.( + ) t.total_size_bid size
  | Sell ->
    t.asks <- Map.set t.asks ~key ~data:order;
    Hashtbl.set t.id_index ~key:id ~data:(Sell, price);
    t.total_size_ask <- Size.( + ) t.total_size_ask size
;;

let remove' t id =
  match Hashtbl.find t.id_index id with
  | None -> None
  | Some (side, price) ->
    let key = price, id in
    Hashtbl.remove t.id_index id;
    let order =
      match side with
      | Buy ->
        let order = Map.find t.bids key in
        t.bids <- Map.remove t.bids key;
        order
      | Sell ->
        let order = Map.find t.asks key in
        t.asks <- Map.remove t.asks key;
        order
    in
    Option.iter order ~f:(fun order ->
      Hashtbl.decr
        t.resting_counts
        (Order.participant order)
        ~remove_if_zero:true;
      let size = Order.remaining_size order in
      match side with
      | Buy -> t.total_size_bid <- Size.( - ) t.total_size_bid size
      | Sell -> t.total_size_ask <- Size.( - ) t.total_size_ask size);
    order
;;

let remove t id = ignore (remove' t id : Order.t option)

(* The matching engine reduces a resting order's [remaining_size] in place
   when it partially fills (it does not go through [add]/[remove']), so it
   reports the reduction here to keep the running side totals accurate. [by]
   is the fill size; the order's own side selects which total to decrement. *)
let record_resting_fill t order ~by =
  match Order.side order with
  | Buy -> t.total_size_bid <- Size.( - ) t.total_size_bid by
  | Sell -> t.total_size_ask <- Size.( - ) t.total_size_ask by
;;

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

(* O(1): read the running total maintained in [add]/[remove']/
   [record_resting_fill], rather than folding the whole side. *)
let total_resting_size t side =
  match side with
  | Side.Buy -> t.total_size_bid
  | Side.Sell -> t.total_size_ask
;;

let resting_count_by_participant t =
  Participant.Map.of_alist_exn (Hashtbl.to_alist t.resting_counts)
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

(* Aggregate this side's resting orders into one [Level.t] per distinct price
   (total size at that price), matching the [Book.t] contract and agreeing
   with [best_bid_offer], which aggregates the top level the same way.

   [orders_on_side] already yields the orders in book order — asks ascending
   (lowest price first), bids descending (the [Bid_map] key reverses price) —
   with equal-price orders adjacent. So neither side needs a sort or a
   reverse: [List.group] splits the already-ordered list into runs of equal
   price (one linear pass), and each run collapses to a level whose size is
   the sum of the run's remaining sizes. *)
let snapshot_side t (side : Side.t) : Level.t list =
  orders_on_side t side
  |> List.group ~break:(fun order1 order2 ->
    not (Price.equal (Order.price order1) (Order.price order2)))
  |> List.map ~f:(fun orders_at_price ->
    (* [List.group] never yields an empty group, so [hd_exn] is safe; every
       order in the run shares one price. *)
    let price = Order.price (List.hd_exn orders_at_price) in
    let size =
      List.fold orders_at_price ~init:Size.zero ~f:(fun total order ->
        Size.( + ) total (Order.remaining_size order))
    in
    { Level.price; size })
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
