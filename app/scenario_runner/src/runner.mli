(** Glue that boots a scenario into a running exchange + ecosystem of bots. *)

open! Core
open! Async
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

(** Boot the exchange on [port], spin up the oracle/news/bots described by
    [config], and return a deferred that resolves only when the server is
    closed. The deferred for each bot's tick loop is leaked via
    [don't_wait_for]. *)
val run : Scenario_config.t -> port:int -> seed:int -> unit Deferred.t

(** Bring up a single bot against an already-running exchange: open its own
    RPC connection, log in, subscribe to its session feed (and market-data
    feed if the spec asks), and start its tick loop. Used by {!run}, and by
    the dashboard's bot launcher to start a bot against a live exchange.

    Resolves once the bot is up, returning a [stop] function. Calling [stop]
    is a kill switch: it halts the tick loop, cancels every order the bot has
    resting (so it leaves no footprint), and closes the connection. *)
val start_bot
  :  where_to_connect:Tcp.Where_to_connect.inet
  -> oracle:Fundamental_oracle.t
  -> Bot_spec.t
  -> (unit -> unit Deferred.t) Deferred.t
