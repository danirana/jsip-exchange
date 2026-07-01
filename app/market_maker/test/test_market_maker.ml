(** Tests for the market maker bot. *)

open! Core
open Jsip_types
open Jsip_market_maker
open Jsip_test_harness

(* The market maker bot is exercised end-to-end by the scenario runner. This
   is a lightweight smoke test that the module builds and a config can be
   constructed; behavioral tests — seeding a symmetric ladder, inventory skew
   on fills — are still a TODO. *)
let%expect_test "config can be constructed" =
  let (_ : Market_maker_bot.Config.t) =
    Market_maker_bot.Config.create
      ~symbol:Harness.aapl
      ~fair_value_cents:15000
      ~half_spread_cents:10
      ~size_per_level:100
      ~num_levels:3
      ~client_order_id:(Client_order_id.of_int 1)
      ~inventory_skew_cents_per_share:5
  in
  print_endline Market_maker_bot.name;
  [%expect {| market_maker |}]
;;
