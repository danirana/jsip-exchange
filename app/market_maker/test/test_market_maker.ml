(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_test_harness
open Jsip_market_maker

(* open E2e_helpers *)
open Jsip_types

let default_config : Market_maker.Config.t =
  { participant = Harness.market_maker
  ; symbol = Harness.aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  ; client_order_id = Client_order_id.of_int 1
  ; inventory_skew_cents_per_share = 5
  }
;;

(* let%expect_test "seed_book: places symmetric bids and asks around fair
   value" = with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
   let%bind mm = connect_as ~port Harness.market_maker in let%bind () =
   Market_maker.seed_book default_config (connection mm)
   ~fair_value_cents:default_config.fair_value_cents in
   [%expect {| [MarketMaker] ACCEPTED id=1 AAPL BUY 100@$149.90 DAY [MarketMaker] ACCEPTED id=2 AAPL SELL 100@$150.10 DAY [MarketMaker] ACCEPTED id=3 AAPL BUY 100@$149.89 DAY [MarketMaker] ACCEPTED id=4 AAPL SELL 100@$150.11 DAY [MarketMaker] ACCEPTED id=5 AAPL BUY 100@$149.88 DAY [MarketMaker] ACCEPTED id=6 AAPL SELL 100@$150.12 DAY |}];
   return ()) ;;
*)

(* let%expect_test "2a test" = let print_current_state () = let inv_val =
   Option.value (Hashtbl.find inventory Harness.aapl) ~default:0 in let
   live_ids = Hashtbl.keys active_orders |> List.map
   ~f:Client_order_id.to_int |> List.sort ~compare:Int.compare in print_s
   [%message "" (inv_val : int) (live_ids : int list) ] in let inventory =
   Symbol.Table.create () in let active_orders = Client_order_id.Table.create
   () in

   print_endline "--- Initial Clean State ---"; print_current_state ();

   (*an Order_accept Event arriving on the session feed *) print_endline
   "\n--- Event 1: Market Maker Order Accepted ---"; let my_order_id =
   Client_order_id.of_int 42 in Hashtbl.set active_orders ~key:my_order_id
   ~data:100; print_current_state ();

   [%expect {| --- Initial Clean State --- ((inv_val  0) (live_ids ())) --- Event 1: Market Maker Order Accepted --- ((inv_val  0) (live_ids (42))) |}]
   in return () ;; *)
