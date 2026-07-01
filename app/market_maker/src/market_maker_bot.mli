open! Core
open! Async
open Jsip_types
module Bot_runtime : module type of Jsip_bot_runtime.Bot_runtime
module Context = Bot_runtime.Context

module Market_maker_bot : sig
  module Config : sig
    type t =
      { symbol : Symbol.t
      ; fair_value_cents : int
      ; half_spread_cents : int
      ; size_per_level : int
      ; num_levels : int
      ; client_order_id : Client_order_id.t
      ; inventory_skew_cents_per_share : int
      }
    [@@deriving sexp_of]
  end

  val name : string
  val inventory : int Symbol.Table.t
  val active_orders : int Client_order_id.Table.t

  val seed_book
    :  Config.t
    -> Context.t
    -> fair_value_cents:int
    -> unit Deferred.t

  val update_inventory : Fill.t -> is_aggressor:bool -> fill_size:int -> int

  val update_active_orders
    :  my_client_order_id:Client_order_id.t
    -> fill_size:int
    -> unit

  val cancel_and_re_quote
    :  Config.t
    -> Context.t
    -> new_inventory:int
    -> unit

  val on_start : Config.t -> Context.t -> unit Deferred.t
  val on_tick : Config.t -> Context.t -> unit Deferred.t
  val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t
end
