open! Core
open Jsip_types

module Trade_report = struct
  type t =
    { symbol : Symbol.t
    ; price : Price.t
    }
  [@@deriving sexp, bin_io]
end

(* The open position for a single (participant, symbol) pair.

   Prices are kept as integer cents rather than [Price.t] because rolling an
   average and netting a close both need division, which [Price.t] does not
   provide. We convert back to [Price.t] only at the {!summary} boundary. *)
module Position = struct
  type t =
    { shares : int (* signed: positive long, negative short *)
    ; avg_entry_cents : int
        (* average entry price of the open position; 0 when flat *)
    ; realized_cents : int
    }
  [@@deriving sexp_of]

  let empty = { shares = 0; avg_entry_cents = 0; realized_cents = 0 }

  (* [+1] for a positive count, [-1] otherwise. Only called on nonzero
     counts, so the [else] branch always means "negative". *)
  let unit_sign n = if n > 0 then 1 else -1

  (* Apply one side of a trade — [qty] shares (always positive) at
     [price_cents] on [side] — via the average-cost method:

     - opening from flat, or adding in the same direction, rolls the
       share-weighted average entry price;
     - reducing realizes P&L on the closed shares against the average entry
       price and leaves that average unchanged for any remainder;
     - crossing through zero closes the whole old position (realizing on it)
       and opens a fresh one at the trade price. *)
  let apply t ~side ~price_cents ~qty =
    let signed_qty = Side.sign side * qty in
    if t.shares = 0
    then { t with shares = signed_qty; avg_entry_cents = price_cents }
    else if Bool.equal (t.shares > 0) (signed_qty > 0)
    then (
      let old_qty = abs t.shares in
      let new_qty = old_qty + qty in
      let avg_entry_cents =
        ((t.avg_entry_cents * old_qty) + (price_cents * qty)) / new_qty
      in
      { t with shares = t.shares + signed_qty; avg_entry_cents })
    else (
      let closing_qty = Int.min (abs t.shares) qty in
      let realized_cents =
        t.realized_cents
        + (unit_sign t.shares
           * (price_cents - t.avg_entry_cents)
           * closing_qty)
      in
      let shares = t.shares + signed_qty in
      if shares = 0
      then { shares; avg_entry_cents = 0; realized_cents }
      else if Bool.equal (shares > 0) (t.shares > 0)
      then
        (* partial close: the remainder keeps its average entry price *)
        { t with shares; realized_cents }
      else
        (* flip: old position closed, remainder opens at the trade price *)
        { shares; avg_entry_cents = price_cents; realized_cents })
  ;;
end

(* Positions are keyed by (participant, symbol). *)
module Key = struct
  module T = struct
    type t = Participant.t * Symbol.t [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t =
  { positions : Position.t Map.M(Key).t
  ; reference_cents : int Map.M(Symbol).t
  }
[@@deriving sexp_of]

let empty =
  { positions = Map.empty (module Key)
  ; reference_cents = Map.empty (module Symbol)
  }
;;

let apply_one t ~participant ~symbol ~side ~price_cents ~qty =
  let key = participant, symbol in
  let position =
    Map.find t.positions key |> Option.value ~default:Position.empty
  in
  let position = Position.apply position ~side ~price_cents ~qty in
  { t with positions = Map.set t.positions ~key ~data:position }
;;

let apply_fill t (fill : Fill.t) =
  let price_cents = Price.to_int_cents fill.price in
  let qty = Size.to_int fill.size in
  let t =
    apply_one
      t
      ~participant:fill.aggressor_participant
      ~symbol:fill.symbol
      ~side:fill.aggressor_side
      ~price_cents
      ~qty
  in
  apply_one
    t
    ~participant:fill.resting_participant
    ~symbol:fill.symbol
    ~side:(Side.flip fill.aggressor_side)
    ~price_cents
    ~qty
;;

let apply_trade_report t (report : Trade_report.t) =
  { t with
    reference_cents =
      Map.set
        t.reference_cents
        ~key:report.symbol
        ~data:(Price.to_int_cents report.price)
  }
;;

module Summary = struct
  module Per_symbol = struct
    type t =
      { symbol : Symbol.t
      ; position : int
      ; average_entry_price : Price.t option
      ; reference_price : Price.t option
      ; realized_cents : int
      ; unrealized_cents : int
      }
    [@@deriving sexp_of]
  end

  type t =
    { per_symbol : Per_symbol.t list
    ; realized_cents : int
    ; unrealized_cents : int
    }
  [@@deriving sexp_of]

  let total_cents t = t.realized_cents + t.unrealized_cents

  (* Signed dollar formatting, e.g. [-12345] -> ["-$123.45"]. Mirrors
     {!Jsip_types.Price.to_string_dollar}, which we can't reuse directly
     because P&L cents can be negative and are not [Price.t] values. *)
  let dollars cents =
    let sign = if cents < 0 then "-" else "" in
    let cents = abs cents in
    sprintf "%s$%d.%02d" sign (cents / 100) (cents % 100)
  ;;

  let price_opt = function
    | None -> "-"
    | Some price -> Price.to_string_dollar price
  ;;

  let to_string_hum t =
    let rows =
      List.map t.per_symbol ~f:(fun (s : Per_symbol.t) ->
        [%string
          "  %{s.symbol#Symbol}: pos=%{s.position#Int} avg=%{price_opt \
           s.average_entry_price} ref=%{price_opt s.reference_price} \
           realized=%{dollars s.realized_cents} unrealized=%{dollars \
           s.unrealized_cents}"])
    in
    let total =
      [%string
        "  total: realized=%{dollars t.realized_cents} unrealized=%{dollars \
         t.unrealized_cents} net=%{dollars (total_cents t)}"]
    in
    String.concat ~sep:"\n" (rows @ [ total ])
  ;;
end

let summary t participant =
  let per_symbol =
    Map.to_alist t.positions
    |> List.filter_map ~f:(fun ((p, symbol), position) ->
      if not (Participant.equal p participant)
      then None
      else (
        let { Position.shares; avg_entry_cents; realized_cents } =
          position
        in
        let reference_cents = Map.find t.reference_cents symbol in
        let unrealized_cents =
          match reference_cents with
          | None -> 0
          | Some ref_cents -> shares * (ref_cents - avg_entry_cents)
        in
        let average_entry_price =
          if shares = 0
          then None
          else Some (Price.of_int_cents avg_entry_cents)
        in
        Some
          { Summary.Per_symbol.symbol
          ; position = shares
          ; average_entry_price
          ; reference_price =
              Option.map reference_cents ~f:Price.of_int_cents
          ; realized_cents
          ; unrealized_cents
          }))
  in
  let realized_cents =
    List.sum (module Int) per_symbol ~f:(fun s -> s.realized_cents)
  in
  let unrealized_cents =
    List.sum (module Int) per_symbol ~f:(fun s -> s.unrealized_cents)
  in
  { Summary.per_symbol; realized_cents; unrealized_cents }
;;
