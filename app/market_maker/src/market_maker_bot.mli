(** A dynamic market-making bot for {!Jsip_bot_runtime.Bot_runtime}.

    Quotes a symmetric ladder of resting orders around a fair value and
    re-centers on every fill, skewing its quotes against accumulated
    inventory so it trades back toward flat. Wire it into the runtime with
    {!Jsip_bot_runtime.Bot_runtime.create}; [app/server/bin/main.ml] and
    [app/scenario_runner] show how a bot is driven over an RPC connection.

    This module satisfies {!Jsip_bot_runtime.Bot_runtime.Bot}. *)

open! Core
open! Async
open Jsip_types
module Bot_runtime : module type of Jsip_bot_runtime.Bot_runtime
module Context = Bot_runtime.Context

(** The market maker's configuration together with its private per-bot state.

    The inventory and live-order tables are created fresh by {!Config.create}
    and mutated in place as the bot trades. They live inside [Config.t]
    because the {!Bot_runtime.Bot} callbacks are handed only a [Config.t] and
    a [Context.t], so the config is the only state the callbacks share. *)
module Config : sig
  type t [@@deriving sexp_of]

  (** Build a config with empty inventory and live-order tables.

      - [fair_value_cents]: center of the initial quote ladder.
      - [half_spread_cents]: distance from the fair value to the nearest
        quotes.
      - [size_per_level]: shares quoted at each level.
      - [num_levels]: number of price levels quoted per side.
      - [client_order_id]: base id from which per-level ids are derived.
      - [inventory_skew_cents_per_share]: how far the fair value shifts per
        share of inventory when re-quoting after a fill. *)
  val create
    :  symbol:Symbol_id.t
    -> fair_value_cents:int
    -> half_spread_cents:int
    -> size_per_level:int
    -> num_levels:int
    -> client_order_id:Client_order_id.t
    -> inventory_skew_cents_per_share:int
    -> t
end

val name : string

(** Seed the initial quote ladder. See {!Bot_runtime.Bot.on_start}. *)
val on_start : Config.t -> Context.t -> unit Deferred.t

(** No-op: this bot reacts to fills, not to the clock. See
    {!Bot_runtime.Bot.on_tick}. *)
val on_tick : Config.t -> Context.t -> unit Deferred.t

(** React to the bot's own accepts, cancels, and fills — updating inventory
    and re-quoting on each fill. See {!Bot_runtime.Bot.on_event}. *)
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t
