open! Core
open Jsip_types
module Rpc = Async_rpc_kernel.Rpc

(* The browser polls this once per second. It returns the whole rolling
   window rather than a single snapshot, so the client can render a
   self-contained time series and a slow/backgrounded tab resyncs cleanly on
   its next poll. A plain request/response [Rpc.Rpc] (not a [Pipe_rpc]) is
   deliberate: the client drives the cadence, so a stalled tab just stops
   asking instead of letting a server-pushed pipe back up. *)
let recent_stats_rpc =
  Rpc.Rpc.create
    ~name:"recent-stats"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Window.bin_t
    ~include_in_error_count:Only_on_exn
;;

(* Browser -> dashboard server: launch one bot of the chosen kind against the
   exchange this dashboard is watching. Unlike [recent_stats_rpc] this
   one *writes*: it can fail (no market data yet, a login collision), so the
   response is [unit Or_error.t] and the client renders either outcome. *)
let launch_bot_rpc =
  Rpc.Rpc.create
    ~name:"launch-bot"
    ~version:0
    ~bin_query:Bot_kind.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

(* Stop one launched bot by participant (kill switch: halt it and flatten its
   orders). [Error] if the dashboard isn't tracking that participant. *)
let stop_bot_rpc =
  Rpc.Rpc.create
    ~name:"stop-bot"
    ~version:0
    ~bin_query:Participant.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

(* Full reset: stop every bot the dashboard has launched, flattening each. *)
let reset_bots_rpc =
  Rpc.Rpc.create
    ~name:"reset-bots"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

(* Full exchange wipe: stop every launched bot AND cancel every resting order
   on the exchange (seed market maker, leftover junk, everything) — a fresh
   book without rebooting. *)
let reset_exchange_rpc =
  Rpc.Rpc.create
    ~name:"reset-exchange"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

(* The bots the dashboard is currently tracking, for the client's stop UI.
   Polled like [recent_stats_rpc]. *)
let running_bots_rpc =
  Rpc.Rpc.create
    ~name:"running-bots"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: Running_bot.t list]
    ~include_in_error_count:Only_on_exn
;;
