(** RPCs shared between the dashboard server and its browser client.

    Only one: the client polls {!recent_stats_rpc} once per second and
    renders the returned {!Window.t}. The exchange's own [stats_rpc] is a
    server-push pipe consumed by the dashboard *server*; the browser never
    touches it directly (it can't open a raw TCP connection, and a polled
    request/response survives a backgrounded tab where a pushed pipe would
    not). *)

open! Core
open Jsip_types
module Rpc = Async_rpc_kernel.Rpc

val recent_stats_rpc : (unit, Window.t) Rpc.Rpc.t

(** Launch a bot of the given kind against the watched exchange. [Ok ()] once
    the bot has connected and started; [Error _] if it couldn't (e.g. the
    dashboard has no market data yet, so it doesn't know what symbols to
    trade). See {!Bot_kind.t}. *)
val launch_bot_rpc : (Bot_kind.t, unit Or_error.t) Rpc.Rpc.t

(** Stop one launched bot by participant: halt it and cancel every order it
    has resting. [Error] if the dashboard isn't tracking that participant. *)
val stop_bot_rpc : (Participant.t, unit Or_error.t) Rpc.Rpc.t

(** Stop every bot the dashboard has launched, flattening each — a full reset
    back to just the exchange's own liquidity. *)
val reset_bots_rpc : (unit, unit Or_error.t) Rpc.Rpc.t

(** Full exchange wipe: stop every launched bot AND cancel every resting
    order on the exchange (including the seed market maker and leftover
    junk), returning to a fresh book without a reboot. *)
val reset_exchange_rpc : (unit, unit Or_error.t) Rpc.Rpc.t

(** The bots the dashboard is currently tracking. Polled by the client to
    render its stop controls. See {!Running_bot.t}. *)
val running_bots_rpc : (unit, Running_bot.t list) Rpc.Rpc.t
