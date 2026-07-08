open! Core
open Bonsai_web
open Jsip_types
module Latency_summary = Jsip_dashboard_controller.Controller.Latency_summary

(* Value formatting shared by the panes. Kept tiny and pure. *)
module Fmt = struct
  (* One OCaml heap word is 8 bytes on 64-bit; report memory in MB so the
     numbers stay human across the range a bot pushes them to. *)
  let bytes_per_word = 8
  let words_to_mb words = Float.of_int (words * bytes_per_word) /. 1_000_000.
  let mb words = Float.to_string_hum ~decimals:1 (words_to_mb words) ^ " MB"

  (* A percentile: the bucket's upper edge, or — for the overflow bucket — a
     "slower than the largest edge" marker. An empty window shows a dash. *)
  let percentile ~count span =
    match count, span with
    | 0, _ -> "—"
    | _, Some span -> Time_ns.Span.to_string span
    | _, None -> "≥ 1s"
  ;;

  let bucket_label : Time_ns.Span.t option -> string = function
    | Some span -> Time_ns.Span.to_string span
    | None -> "∞"
  ;;

  let price = function
    | None -> "—"
    | Some price -> Price.to_string_dollar price
  ;;

  let size = function
    | None -> "—"
    | Some size -> Int.to_string (Size.to_int size)
  ;;

  (* Compact span for the engine tiles: one decimal in the largest fitting
     unit ([14.3ms], not [14.301068ms]), so a gap never overflows its box. *)
  let span s = Time_ns.Span.to_string_hum ~decimals:1 s
end

(* A labelled numeric tile, shared by the latency, pipe, and engine panes. *)
let stat_tile ~label ~value =
  {%html|
    <div %{Styles.attr Styles.tile}>
      <span %{Styles.attr Styles.tile_label}>#{label}</span>
      <span %{Styles.attr Styles.tile_value}>#{value}</span>
    </div>
  |}
;;

(* A vertical bar in a fixed-height row. [height_pct] is clamped to [0, 100]
   so a stray value can't overflow the pane. sprintf handles the literal '%'. *)
let bar ~height_pct ~color =
  let height_pct = Int.clamp_exn height_pct ~min:0 ~max:100 in
  let style =
    Printf.sprintf
      "flex:1; min-width:2px; height:%d%%; background:%s; border-radius:1px \
       1px 0 0;"
      height_pct
      color
  in
  {%html|<div %{Styles.attr style}></div>|}
;;

let scaled_pct ~value ~max_value =
  if Float.( <= ) max_value 0.
  then 0
  else Float.to_int (value /. max_value *. 100.)
;;

module Memory = struct
  (* Live OCaml-heap usage as a sparkline over the window, with the current
     value and the window's own range called out. A flat line vs. a steady
     climb is the whole point of this pane. Scaling bars to the peak would
     make a stable-but-high line look identical to a flat-full block — you
     couldn't see a small climb — so we auto-range: the window's [min, max]
     maps onto [min_bar_pct, 100], turning even a small increase into a
     visible ramp. The absolute level still reads off the big number and the
     range label; the bars carry the shape. *)

  (* The shortest bar still gets this height, so the low end of the range
     stays visible rather than collapsing to nothing. *)
  let min_bar_pct = 12

  let view ~series ~latest_words ~top_words:_ =
    let min_words =
      List.min_elt series ~compare:Int.compare |> Option.value ~default:0
    in
    let max_words =
      List.max_elt series ~compare:Int.compare |> Option.value ~default:0
    in
    let span_words = max_words - min_words in
    (* A flat window (no range) draws a mid line; otherwise map [min, max]
       onto [min_bar_pct, 100]. *)
    let height_pct words =
      if span_words <= 0
      then 50
      else
        min_bar_pct + ((100 - min_bar_pct) * (words - min_words) / span_words)
    in
    let bars =
      List.map series ~f:(fun words ->
        bar ~height_pct:(height_pct words) ~color:"var(--color-accent)")
    in
    {%html|
      <div %{Styles.attr Styles.pane}>
        <div %{Styles.attr Styles.pane_title}>process memory · live_words</div>
        <div>
          <span %{Styles.attr Styles.stat_value}>#{Fmt.mb latest_words}</span>
        </div>
        <div %{Styles.attr Styles.bars_row}>*{bars}</div>
        <div %{Styles.attr Styles.bars_axis}>
          <span>#{Int.to_string (List.length series)}s ago</span>
          <span>range #{Fmt.mb min_words}–#{Fmt.mb max_words}</span>
          <span>now</span>
        </div>
      </div>
    |}
  ;;
end

module Latency = struct
  (* Submit or cancel latency: p50/p90/p99 tiles above the merged histogram.
     Bars are scaled linearly to the busiest bucket in the window. *)
  let tile ~label ~value =
    {%html|
      <div %{Styles.attr Styles.tile}>
        <span %{Styles.attr Styles.tile_label}>#{label}</span>
        <span %{Styles.attr Styles.tile_value}>#{value}</span>
      </div>
    |}
  ;;

  let view ~title (summary : Latency_summary.t) =
    let count = summary.count in
    let max_count =
      List.map summary.buckets ~f:(fun b -> b.count)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:1
      |> Int.max 1
    in
    let bars =
      List.map summary.buckets ~f:(fun { count = bucket_count; _ } ->
        bar
          ~height_pct:
            (scaled_pct
               ~value:(Float.of_int bucket_count)
               ~max_value:(Float.of_int max_count))
          ~color:"var(--color-accent-dim)")
    in
    let edge_labels =
      [ List.hd summary.buckets; List.last summary.buckets ]
      |> List.filter_map ~f:Fn.id
      |> List.map ~f:(fun b ->
        {%html|<span>#{Fmt.bucket_label b.upper_bound}</span>|})
    in
    {%html|
      <div %{Styles.attr Styles.pane}>
        <div %{Styles.attr Styles.pane_title}>#{title}</div>
        <div %{Styles.attr Styles.tile_row}>
          %{tile ~label:"p50" ~value:(Fmt.percentile ~count summary.p50)}
          %{tile ~label:"p90" ~value:(Fmt.percentile ~count summary.p90)}
          %{tile ~label:"p99" ~value:(Fmt.percentile ~count summary.p99)}
        </div>
        <div %{Styles.attr Styles.bars_row}>*{bars}</div>
        <div %{Styles.attr Styles.bars_axis}>*{edge_labels}</div>
      </div>
    |}
  ;;
end

module Book = struct
  (* The hero of the dashboard: a best-bid/offer board across every traded
     symbol. Buyers (the bid) sit on the left in green, sellers (the ask) on
     the right in red, the spread runs down the middle, and a per-symbol bar
     shows which side of the resting book is stacked deeper. This is the pane
     a trader watches; the infrastructure panes sit below it. *)

  (* The bid's share of the visible resting depth, in [0., 1.]. This is the
     split point of the imbalance bar: 0.5 draws a balanced book, values
     toward 1.0 draw a bid-heavy (buyers-stacked) book, toward 0.0 an
     ask-heavy one. See the Learn-by-Doing note where this is used. *)
  let bid_fraction ~(bid : Size.t) ~(ask : Size.t) : float =
    let bid = Size.to_int bid in
    let ask = Size.to_int ask in
    let total = bid + ask in
    (* An empty book has no skew to show, so draw it balanced rather than
       dividing by zero. With any depth, the bid's share lands in [0., 1.]: a
       one-sided book pins the bar hard to 1.0 or 0.0. *)
    if total = 0 then 0.5 else Float.of_int bid /. Float.of_int total
  ;;

  (* A single track split green|red by [bid_fraction]. The two halves butt
     together to fill the fixed-width track, so the bar never reflows. *)
  let imbalance_bar ~bid ~ask =
    let bid_pct =
      Float.to_int (bid_fraction ~bid ~ask *. 100.)
      |> Int.clamp_exn ~min:0 ~max:100
    in
    let ask_pct = 100 - bid_pct in
    {%html|
      <div %{Styles.attr Styles.imbalance_track}>
        <div %{Styles.attr
                 (Styles.imbalance_fill
                    ~pct:bid_pct
                    ~color:"var(--color-bid)")}></div>
        <div %{Styles.attr
                 (Styles.imbalance_fill
                    ~pct:ask_pct
                    ~color:"var(--color-ask)")}></div>
      </div>
    |}
  ;;

  let row (depth : Exchange_stats.Book_depth.t) =
    let bbo = depth.bbo in
    {%html|
      <tr %{Styles.attr Styles.book_row}>
        <td %{Styles.attr Styles.book_sym}>#{Symbol.to_string depth.symbol}</td>
        <td %{Styles.attr Styles.book_size_bid}>#{Fmt.size (Bbo.size bbo Side.Buy)}</td>
        <td %{Styles.attr Styles.book_price_bid}>#{Fmt.price (Bbo.price bbo Side.Buy)}</td>
        <td %{Styles.attr Styles.book_spread}>#{Fmt.price (Bbo.spread bbo)}</td>
        <td %{Styles.attr Styles.book_price_ask}>#{Fmt.price (Bbo.price bbo Side.Sell)}</td>
        <td %{Styles.attr Styles.book_size_ask}>#{Fmt.size (Bbo.size bbo Side.Sell)}</td>
        <td %{Styles.attr Styles.imbalance_cell}>
          %{imbalance_bar
              ~bid:depth.resting_size_bid
              ~ask:depth.resting_size_ask}
        </td>
      </tr>
    |}
  ;;

  let view ~rows =
    let body =
      match rows with
      | [] ->
        [ {%html|<tr><td %{Styles.attr Styles.empty}>no symbols trading</td></tr>|}
        ]
      | rows -> List.map rows ~f:row
    in
    {%html|
      <div %{Styles.attr Styles.pane}>
        <div %{Styles.attr Styles.pane_title}>order book · best bid / offer</div>
        <table %{Styles.attr Styles.book_table}>
          <thead>
            <tr>
              <th %{Styles.attr Styles.th_left}>sym</th>
              <th %{Styles.attr Styles.th}>bid sz</th>
              <th %{Styles.attr Styles.th}>bid</th>
              <th %{Styles.attr Styles.th_center}>spread</th>
              <th %{Styles.attr Styles.th}>ask</th>
              <th %{Styles.attr Styles.th}>ask sz</th>
              <th %{Styles.attr Styles.th_left}>depth</th>
            </tr>
          </thead>
          <tbody>*{body}</tbody>
        </table>
      </div>
    |}
  ;;
end

module Pipe_occupancy = struct
  (* Worst queue depth of each outbound-pipe family. A slow consumer is the
     one whose depth climbs while the others stay near zero — the
     [slowest session] line names it. This is the pane that catches the
     slow-consumers bot. *)
  let view ~series (occupancy : Exchange_stats.Pipe_occupancy.t option) =
    let market, audit, session, slowest =
      match occupancy with
      | None -> "—", "—", "—", "—"
      | Some o ->
        ( Int.to_string o.market_data_max
        , Int.to_string o.audit_max
        , Int.to_string o.session_max
        , (match o.slowest_session with
           | Some participant -> Participant.to_string participant
           | None -> "none") )
    in
    (* The worst-pipe depth over the window. In a healthy exchange every pipe
       drains as fast as it fills, so this line sits flat at zero; a slow
       consumer makes it climb. Warn-colored because any nonzero value here
       is the early signal we built this pane to catch. *)
    let peak =
      List.max_elt series ~compare:Int.compare
      |> Option.value ~default:0
      |> Int.max 1
    in
    let bars =
      List.map series ~f:(fun depth ->
        bar
          ~height_pct:
            (scaled_pct
               ~value:(Float.of_int depth)
               ~max_value:(Float.of_int peak))
          ~color:"var(--color-warn)")
    in
    {%html|
      <div %{Styles.attr Styles.pane}>
        <div %{Styles.attr Styles.pane_title}>pipe occupancy · max queue depth</div>
        <div %{Styles.attr Styles.tile_row}>
          %{stat_tile ~label:"market data" ~value:market}
          %{stat_tile ~label:"audit" ~value:audit}
          %{stat_tile ~label:"session" ~value:session}
        </div>
        <div %{Styles.attr Styles.bars_row}>*{bars}</div>
        <div %{Styles.attr Styles.bars_axis}>
          <span>slowest session: #{slowest}</span>
          <span>peak #{Int.to_string (List.max_elt series ~compare:Int.compare |> Option.value ~default:0)}</span>
        </div>
      </div>
    |}
  ;;
end

module Participants = struct
  (* Per-participant order rate and current resting-order count. Under the
     pathological bots one row runs away from the rest. *)
  let row (activity : Exchange_stats.Participant_activity.t) =
    {%html|
      <tr>
        <td %{Styles.attr Styles.td_left}>#{Participant.to_string activity.participant}</td>
        <td %{Styles.attr Styles.td}>#{Int.to_string activity.orders_last_interval}</td>
        <td %{Styles.attr Styles.td}>#{Int.to_string activity.resting_orders}</td>
      </tr>
    |}
  ;;

  let view ~rows =
    let body =
      match rows with
      | [] ->
        [ {%html|<tr><td %{Styles.attr Styles.empty}>no participants</td></tr>|}
        ]
      | rows -> List.map rows ~f:row
    in
    {%html|
      <div %{Styles.attr Styles.pane}>
        <div %{Styles.attr Styles.pane_title}>participants</div>
        <table %{Styles.attr Styles.table}>
          <thead>
            <tr>
              <th %{Styles.attr Styles.th_left}>participant</th>
              <th %{Styles.attr Styles.th}>orders/s</th>
              <th %{Styles.attr Styles.th}>resting</th>
            </tr>
          </thead>
          <tbody>*{body}</tbody>
        </table>
      </div>
    |}
  ;;
end

module Engine = struct
  (* Matching-loop health: the request backlog waiting to be matched, and the
     gap between successive drain iterations. Empty queue + near-zero gaps =
     keeping up; deep queue + growing gaps = falling behind. When it is
     falling behind we swap the pane to the [--color-bad]-bordered variant
     and show a status chip, so a glance catches it. *)

  (* [~falling_behind] is the saturation verdict, computed once in
     {!Jsip_dashboard_controller.Controller.is_falling_behind} (shared with
     the [verify_stats] observer and unit-tested there) from the latest
     snapshot. This pane only renders it: alert border + chip when set. *)
  let view ~falling_behind (engine : Exchange_stats.Engine_busyness.t option)
    =
    let pane_style, chip =
      match engine with
      | None -> Styles.pane, {%html|<span></span>|}
      | Some (_ : Exchange_stats.Engine_busyness.t) ->
        let style, label =
          if falling_behind
          then Styles.pane_alert, (Styles.status_bad, "falling behind")
          else Styles.pane, (Styles.status_good, "keeping up")
        in
        let chip_style, chip_label = label in
        style, {%html|<span %{Styles.attr chip_style}>#{chip_label}</span>|}
    in
    let depth, max_gap, mean_gap =
      match engine with
      | None -> "—", "—", "—"
      | Some e ->
        Int.to_string e.queue_depth, Fmt.span e.max_gap, Fmt.span e.mean_gap
    in
    (* Taller tile than the shared [stat_tile]: it centers a larger value so
       the three boxes fill the stretched pane instead of leaving space
       below. *)
    let tile ~label ~value =
      {%html|
        <div %{Styles.attr Styles.engine_tile}>
          <span %{Styles.attr Styles.tile_label}>#{label}</span>
          <span %{Styles.attr Styles.engine_tile_value}>#{value}</span>
        </div>
      |}
    in
    {%html|
      <div %{Styles.attr pane_style}>
        <div %{Styles.attr Styles.title_row}>
          <span %{Styles.attr Styles.pane_title}>matching engine</span>
          %{chip}
        </div>
        <div %{Styles.attr Styles.engine_tile_row}>
          %{tile ~label:"queue depth" ~value:depth}
          %{tile ~label:"max gap" ~value:max_gap}
          %{tile ~label:"mean gap" ~value:mean_gap}
        </div>
      </div>
    |}
  ;;
end
