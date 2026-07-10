open! Core
open Jsip_types

(* By default a symbol id renders as its integer — the phase-1 behaviour,
   kept for server-side and test callers. Consumers that hold a
   {!Symbol_directory} pass [~render_symbol:(Symbol_directory.label dir)] to
   show names instead. Resolution lives here (and in the client/monitor),
   never in [lib/types]. *)
let default_render_symbol = Symbol_id.to_string

(* Mirror of [Fill.to_string], but the symbol id is resolved through
   [render_symbol]. Kept here rather than in [lib/types] because turning an
   id into a name is a consumer concern; the wire type stays int-only. *)
let format_fill ~render_symbol (fill : Fill.t) =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s(%s, cid=%s) %s resting=%s(%s, cid=%s)"
    fill.fill_id
    (render_symbol fill.symbol)
    (Price.to_string_dollar fill.price)
    (Size.to_int fill.size)
    (Order_id.to_string fill.aggressor_order_id)
    (Participant.to_string fill.aggressor_participant)
    (Client_order_id.to_string fill.aggressor_client_order_id)
    (Side.to_string fill.aggressor_side)
    (Order_id.to_string fill.resting_order_id)
    (Participant.to_string fill.resting_participant)
    (Client_order_id.to_string fill.resting_client_order_id)
;;

let format_event ?(render_symbol = default_render_symbol) = function
  | Exchange_event.Order_accept { order_id; request } ->
    sprintf
      "ACCEPTED id=%s %s %s %d@%s %s"
      (Order_id.to_string order_id)
      (render_symbol request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill ->
    let fill_str = format_fill ~render_symbol fill in
    [%string "FILL %{fill_str}"]
  | Order_cancel
      { order_id
      ; participant = _
      ; symbol
      ; remaining_size
      ; reason
      ; client_order_id
      } ->
    sprintf
      "CANCELLED id=%s %s remaining=%d reason=%s cid=%d"
      (Order_id.to_string order_id)
      (render_symbol symbol)
      (Size.to_int remaining_size)
      (Cancel_reason.to_string reason)
      (Client_order_id.to_int client_order_id)
  | Order_reject { request; reason } ->
    sprintf
      "REJECTED %s %s %d@%s reason=%s"
      (render_symbol request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Best_bid_offer_update { symbol; bbo } ->
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    let symbol = render_symbol symbol in
    [%string "BBO %{symbol} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol; price; size } ->
    let size = Size.to_int size in
    let symbol = render_symbol symbol in
    [%string "TRADE %{symbol} %{price#Price} x%{size#Int}"]
  | Cancel_reject { participant = _; client_order_id; reason } ->
    sprintf
      "CANCEL_REJECT cl_ord_id=%s reason=%s"
      (Client_order_id.to_string client_order_id)
      reason
;;

let format_events ?render_symbol events =
  List.map events ~f:(format_event ?render_symbol) |> String.concat ~sep:"\n"
;;

(* Directory-aware book render: [Book.to_string] with the header symbol
   resolved via [render_symbol]. The sides and BBO are rendered by the
   [lib/types] pretty-printers, which carry no symbol, so nothing is
   duplicated beyond the one-line header. *)
let format_book ?(render_symbol = default_render_symbol) (book : Book.t) =
  let format_side label levels =
    match levels with
    | [] -> [%string "  %{label}: (empty)"]
    | _ ->
      let lines =
        List.map levels ~f:(fun level -> [%string "    %{level#Level}"])
        |> String.concat ~sep:"\n"
      in
      [%string "  %{label}:\n%{lines}"]
  in
  let symbol = render_symbol book.symbol in
  String.concat
    ~sep:"\n"
    [ [%string "=== %{symbol} ==="]
    ; format_side "BIDS" book.bids
    ; format_side "ASKS" book.asks
    ; [%string "  BBO: %{book.bbo#Bbo}"]
    ]
;;

(* Directory-aware analogue of [Fill.to_participant_view]: the "You
   bought/sold N <name> at $P" line the client shows for its own fills.
   [None] when [viewer] is neither side of the fill. *)
let fill_participant_view
  ?(render_symbol = default_render_symbol)
  (fill : Fill.t)
  ~viewer
  =
  let participant_side =
    if Participant.equal fill.aggressor_participant viewer
    then Some fill.aggressor_side
    else if Participant.equal fill.resting_participant viewer
    then Some (Side.flip fill.aggressor_side)
    else None
  in
  Option.map participant_side ~f:(fun side ->
    let action =
      match side with Side.Buy -> "bought" | Side.Sell -> "sold"
    in
    let n = Size.to_int fill.size in
    let symbol = render_symbol fill.symbol in
    let price = Price.to_string_dollar fill.price in
    [%string "You %{action} %{n#Int} %{symbol} at %{price}"])
;;
