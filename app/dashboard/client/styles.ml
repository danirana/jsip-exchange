open! Core
open Bonsai_web

(* Style tokens for the dashboard, in one place. Colors and spacing live as
   CSS custom properties on [:root] in index.html; these strings reference
   them with [var(...)] so the palette stays defined once. There is no
   ppx_css here, so a [style="..."] attribute is built explicitly with
   [Vdom.Attr.create]. *)

let attr style = Vdom.Attr.create "style" style

(* The whole page: a fixed status bar over a scrolling column of sections
   that fills the viewport. *)
let page =
  "height:100%; display:flex; flex-direction:column; \
   background:var(--color-bg-0);"
;;

(* Top status bar (tier 1 band). Brand on the left, live/window state pushed
   to the right. Stays put while the sections below scroll. *)
let statusbar =
  "display:flex; align-items:center; gap:var(--space-lg); \
   padding:var(--space-sm) var(--space-lg); background:var(--color-bg-1); \
   border-bottom:1px solid var(--color-border-2); \
   box-shadow:var(--shadow-1); flex:none;"
;;

let brand =
  "display:flex; align-items:center; gap:var(--space-sm); \
   font-size:var(--font-size-md); font-weight:var(--font-weight-bold); \
   color:var(--color-text-primary);"
;;

(* The little accent tile that reads as a logo mark. *)
let brand_mark =
  "width:14px; height:14px; border-radius:var(--radius-sm); \
   background:var(--color-accent); box-shadow:0 0 8px var(--color-accent);"
;;

let brand_sub =
  "font-size:var(--font-size-xs); font-weight:var(--font-weight-medium); \
   color:var(--color-text-quaternary); text-transform:uppercase;"
;;

let statusbar_spacer = "flex:1;"

(* The bot launcher: a select + button + transient result, sitting in the
   status bar. The button is the one loud, primary action on the whole page,
   so it carries the accent fill. *)
let launcher = "display:flex; align-items:center; gap:var(--space-sm);"

let launcher_select =
  "background:var(--color-bg-2); color:var(--color-text-secondary); \
   border:1px solid var(--color-border-2); border-radius:var(--radius-md); \
   padding:3px var(--space-sm); font-size:var(--font-size-xs); \
   font-family:var(--font-sans);"
;;

let launcher_button =
  "background:var(--color-accent); color:lch(98% 0.5 240); border:1px solid \
   var(--color-accent); border-radius:var(--radius-md); padding:3px \
   var(--space-md); font-size:var(--font-size-xs); \
   font-weight:var(--font-weight-semibold); cursor:pointer;"
;;

(* The last-launch readout: green on success, red on failure. Fixed to the
   mono font so a long error message wraps predictably rather than jerking
   the bar's height around. *)
let launch_note color =
  [%string
    "font-size:var(--font-size-xs); color:%{color}; \
     font-family:var(--font-mono); max-width:280px;"]
;;

let launch_ok = launch_note "var(--color-good)"
let launch_err = launch_note "var(--color-bad)"

(* Running-bot chips: one pill per launched bot, each with an ✕ to stop it.
   The row scrolls horizontally rather than wrapping, so a pile of bots never
   grows the status bar's height. *)
let running_area =
  "display:flex; align-items:center; gap:var(--space-xs); overflow-x:auto; \
   max-width:34vw;"
;;

let running_chip =
  "display:flex; align-items:center; gap:var(--space-xs); flex:none; \
   padding:2px 2px 2px var(--space-sm); border-radius:var(--radius-full); \
   background:var(--color-bg-2); border:1px solid var(--color-border-2); \
   font-size:var(--font-size-xs); font-family:var(--font-mono); \
   color:var(--color-text-secondary);"
;;

(* The ✕ inside a chip. A round, quiet button that turns red on hover (via
   the global button:hover brighten) to read as "remove". *)
let running_chip_stop =
  "display:flex; align-items:center; justify-content:center; width:16px; \
   height:16px; border-radius:var(--radius-full); border:none; \
   background:transparent; color:var(--color-text-tertiary); \
   font-size:var(--font-size-sm); line-height:1; cursor:pointer;"
;;

(* Reset-all: a destructive-secondary button. Outlined in the bad color
   instead of filled, so it reads as "danger" without shouting like the
   primary launch button. *)
let reset_button =
  "background:transparent; color:var(--color-bad); border:1px solid \
   var(--color-bad); border-radius:var(--radius-md); padding:3px \
   var(--space-md); font-size:var(--font-size-xs); \
   font-weight:var(--font-weight-semibold); cursor:pointer;"
;;

(* One right-aligned status readout (window length, cadence). Mono so the
   digits don't jitter as they tick. *)
let status_item =
  "display:flex; align-items:center; gap:var(--space-xs); \
   font-size:var(--font-size-xs); color:var(--color-text-tertiary); \
   font-family:var(--font-mono);"
;;

(* The live cluster: a breathing dot plus a LIVE label. [live_dot] takes its
   color token so the connecting state can reuse it in warn. *)
let live_group =
  "display:flex; align-items:center; gap:var(--space-sm); padding:2px \
   var(--space-sm); border-radius:var(--radius-full); \
   background:var(--color-bg-2); border:1px solid var(--color-border-2);"
;;

let live_dot color =
  [%string
    "width:8px; height:8px; border-radius:var(--radius-full); \
     background:%{color}; box-shadow:0 0 6px %{color}; animation:live-pulse \
     2s ease-in-out infinite;"]
;;

let live_label color =
  [%string
    "font-size:var(--font-size-xs); \
     font-weight:var(--font-weight-semibold); color:%{color}; \
     letter-spacing:0;"]
;;

(* The scrolling body: stacked sections, market first, infrastructure last. *)
let main =
  "flex:1; min-height:0; overflow:auto; display:flex; \
   flex-direction:column; gap:var(--space-lg); padding:var(--space-lg);"
;;

(* A quiet uppercase label that heads a band of panes. *)
let section_label =
  "font-size:var(--font-size-xs); text-transform:uppercase; \
   font-weight:var(--font-weight-semibold); \
   color:var(--color-text-quaternary); padding:0 var(--space-xs);"
;;

(* The hero band: the book gets the lion's share, participants ride
   alongside, and both wrap under the book when the viewport gets narrow.
   [flex] lets the band claim its share of the viewport's spare height
   instead of leaving it pooled at the bottom. *)
let hero_row =
  "display:flex; flex-wrap:wrap; gap:var(--space-lg); flex:2 1 auto; \
   min-height:200px;"
;;

let hero_main = "flex:3 1 460px; min-width:0; display:flex;"
let hero_side = "flex:1 1 260px; min-width:0; display:flex;"

(* The infrastructure band: the health panes in a dense auto-fit grid below
   the market. It grows to fill the rest of the viewport, and its rows
   stretch to equal height ([grid-auto-rows: 1fr]) — Grafana-style uniform
   panels — so the charts inside them get taller rather than the page ending
   in dead space. *)
let grid =
  "display:grid; gap:var(--space-md); flex:3 1 0; min-height:0; \
   grid-template-columns:repeat(auto-fit, minmax(300px, 1fr)); \
   grid-auto-rows:minmax(150px, 1fr); align-content:stretch;"
;;

(* One tier-1 surface. Panes never nest, per the design guidance. [width] and
   [flex] let a pane fill both a grid cell and a hero flex column. *)
let pane =
  "background:var(--color-bg-1); border:1px solid var(--color-border-1); \
   border-radius:var(--radius-lg); box-shadow:var(--shadow-2); \
   padding:var(--space-lg); display:flex; flex-direction:column; \
   gap:var(--space-md); min-width:0; width:100%; flex:1 1 auto;"
;;

(* Same surface as [pane], but a [--color-bad] border to flag a pane whose
   metric has crossed into trouble (the engine falling behind). Only the
   border changes, so the pane's contents don't reflow when it trips. *)
let pane_alert = pane ^ " border-color:var(--color-bad);"

let pane_title =
  "font-size:var(--font-size-xs); text-transform:uppercase; \
   letter-spacing:0; color:var(--color-text-tertiary); font-weight:600;"
;;

(* A small status pill next to a pane title. [chip] takes the token name of
   its color; [status_good]/[status_bad] are the two the engine pane uses. *)
let chip color =
  [%string
    "font-size:var(--font-size-xs); font-weight:600; padding:1px 8px; \
     border-radius:999px; color:%{color}; border:1px solid %{color};"]
;;

let status_good = chip "var(--color-good)"
let status_bad = chip "var(--color-bad)"

(* A pane title that sits on one line with its status chip pushed to the far
   end. *)
let title_row =
  "display:flex; align-items:center; justify-content:space-between; \
   gap:var(--space-sm);"
;;

let stat_value =
  "font-family:var(--font-mono); font-size:var(--font-size-xl); \
   color:var(--color-text-primary); line-height:1;"
;;

let stat_unit =
  "font-size:var(--font-size-sm); color:var(--color-text-tertiary);"
;;

(* A row of labelled numeric tiles (the p50/p90/p99 row). *)
let tile_row = "display:flex; gap:var(--space-md);"

let tile =
  "flex:1; display:flex; flex-direction:column; gap:var(--space-xs); \
   padding:var(--space-sm) var(--space-md); background:var(--color-bg-2); \
   border-radius:var(--radius-md); min-width:0;"
;;

let tile_label =
  "font-size:var(--font-size-xs); color:var(--color-text-quaternary);"
;;

let tile_value =
  "font-family:var(--font-mono); font-size:var(--font-size-md); \
   color:var(--color-text-secondary); overflow:hidden; \
   text-overflow:ellipsis; white-space:nowrap;"
;;

(* The engine pane is title + tiles only — no chart below — so on a stretched
   grid row it leaves dead space. Grow its tile row to claim that height and
   center a larger value in each tile, so the three boxes expand to fill it. *)
let engine_tile_row = tile_row ^ " flex:1 1 auto;"
let engine_tile = tile ^ " justify-content:center;"

let engine_tile_value =
  "font-family:var(--font-mono); font-size:var(--font-size-xl); \
   color:var(--color-text-primary); line-height:1.1; overflow:hidden; \
   text-overflow:ellipsis; white-space:nowrap;"
;;

(* Bar charts (memory sparkline, latency histogram). Bars grow upward from a
   baseline row that fills whatever height its pane has to spare (floored at
   72px), so a chart pane fills tall rows with a bigger chart instead of dead
   space. Bars are scaled to the data, so this never reflows as data arrives. *)
let bars_row =
  "display:flex; align-items:flex-end; gap:1px; flex:1 1 auto; \
   min-height:72px; background:var(--color-bg-0); \
   border-radius:var(--radius-sm); padding:var(--space-xs);"
;;

let bars_axis =
  "display:flex; justify-content:space-between; font-size:10px; \
   color:var(--color-text-quaternary); font-family:var(--font-mono);"
;;

(* A dense table for book depth. *)
let table =
  "width:100%; border-collapse:collapse; font-family:var(--font-mono);"
;;

let th =
  "text-align:right; padding:var(--space-xs) var(--space-sm); \
   font-size:var(--font-size-xs); color:var(--color-text-quaternary); \
   font-weight:500; border-bottom:1px solid var(--color-border-1);"
;;

let th_left = th ^ " text-align:left;"
let th_center = th ^ " text-align:center;"

let td =
  "text-align:right; padding:var(--space-xs) var(--space-sm); \
   font-size:var(--font-size-sm); color:var(--color-text-secondary);"
;;

let td_left = td ^ " text-align:left; color:var(--color-text-primary);"

let empty =
  "color:var(--color-text-quaternary); font-size:var(--font-size-sm);"
;;

(* --- The hero book board --------------------------------------------------

   A BBO board: one row per symbol, buyers (bid) on the left in green,
   sellers (ask) on the right in red, the spread down the middle, and an
   imbalance bar showing which side is stacked deeper. Prices are the hero,
   so they run a size larger than the surrounding tables. *)

let book_table =
  "width:100%; border-collapse:collapse; font-family:var(--font-mono);"
;;

(* A hairline under the header row and between symbols keeps a dense board
   readable without boxing every cell. *)
let book_row = "border-top:1px solid var(--color-border-1);"

let book_sym =
  "text-align:left; padding:var(--space-sm); font-size:var(--font-size-md); \
   font-weight:var(--font-weight-semibold); \
   color:var(--color-text-primary); font-family:var(--font-mono);"
;;

(* Bid and ask price cells: the two loud numbers on the board. *)
let book_price side_color =
  [%string
    "text-align:right; padding:var(--space-sm); \
     font-size:var(--font-size-lg); color:%{side_color}; \
     font-family:var(--font-mono);"]
;;

let book_price_bid = book_price "var(--color-bid)"
let book_price_ask = book_price "var(--color-ask)"

(* Size cells sit next to their price, quieter, in the same hue. *)
let book_size side_color =
  [%string
    "text-align:right; padding:var(--space-sm); \
     font-size:var(--font-size-sm); color:%{side_color}; \
     font-family:var(--font-mono);"]
;;

let book_size_bid = book_size "var(--color-bid)"
let book_size_ask = book_size "var(--color-ask)"

(* The spread sits dead center between bid and ask, neutral-toned. *)
let book_spread =
  "text-align:center; padding:var(--space-sm); \
   font-size:var(--font-size-sm); color:var(--color-text-tertiary); \
   font-family:var(--font-mono);"
;;

(* The imbalance bar: a single track split green|red by resting-depth share.
   A wider green half means buyers are stacked deeper than sellers. *)
let imbalance_cell = "padding:var(--space-sm); width:140px;"

let imbalance_track =
  "display:flex; height:8px; border-radius:var(--radius-full); \
   overflow:hidden; background:var(--color-bg-3);"
;;

(* [imbalance_fill] is parameterized by width-percent and side color; the two
   halves butt together to fill the track. *)
let imbalance_fill ~pct ~color =
  [%string "width:%{pct#Int}%; background:%{color}; height:100%;"]
;;
