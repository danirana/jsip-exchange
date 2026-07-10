open! Core
open Jsip_types

(* [Symbol_id.t] is a [private int] minted through [of_int]. These tests pin
   the three things the rest of the exchange relies on: the round-trip
   through [to_int], that [to_string] renders the bare integer (phase 1
   prints ids, not names), and that the underlying int is readable via the
   [:>] coercion the matching engine uses to index its book array.
   Out-of-range *validation* is not tested here — it lives at the engine
   boundary; see [test_matching_engine]'s "rejected: unknown symbol". *)

let%expect_test "of_int / to_int round-trip" =
  List.iter [ 0; 1; 7; 42 ] ~f:(fun i ->
    let id = Symbol_id.of_int i in
    print_s [%message "" ~i:(i : int) ~back:(Symbol_id.to_int id : int)]);
  [%expect
    {|
    ((i 0) (back 0))
    ((i 1) (back 1))
    ((i 7) (back 7))
    ((i 42) (back 42))
    |}]
;;

let%expect_test "to_string renders the integer" =
  List.iter [ 0; 7; 100 ] ~f:(fun i ->
    print_endline (Symbol_id.to_string (Symbol_id.of_int i)));
  [%expect {|
    0
    7
    100
    |}]
;;

let%expect_test "the underlying int is readable by coercion" =
  let id = Symbol_id.of_int 7 in
  (* [private int] lets a reader coerce out the int without a function call —
     this is exactly what the engine does to index [books]. *)
  print_s [%sexp ((id :> int) : int)];
  [%expect {| 7 |}]
;;

let%expect_test "equality follows the underlying int" =
  print_s
    [%sexp
      (Symbol_id.equal (Symbol_id.of_int 3) (Symbol_id.of_int 3) : bool)];
  print_s
    [%sexp
      (Symbol_id.equal (Symbol_id.of_int 3) (Symbol_id.of_int 4) : bool)];
  [%expect {|
    true
    false
    |}]
;;
