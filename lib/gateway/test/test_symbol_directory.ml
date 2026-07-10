open! Core
open Jsip_types
open Jsip_gateway

let directory =
  Symbol_directory.create
    [ Symbol.of_string "AAPL", Symbol_id.of_int 0
    ; Symbol.of_string "TSLA", Symbol_id.of_int 1
    ; Symbol.of_string "GOOG", Symbol_id.of_int 2
    ]
;;

let%expect_test "id_of_name / name_of_id round-trip" =
  let show name =
    let id = Symbol_directory.id_of_name directory (Symbol.of_string name) in
    print_s [%message name ~id:(id : Symbol_id.t option)]
  in
  show "AAPL";
  show "GOOG";
  show "NOPE";
  [%expect
    {|
    (AAPL (id (0)))
    (GOOG (id (2)))
    (NOPE (id ()))
    |}];
  List.iter [ 0; 2; 9 ] ~f:(fun i ->
    let name = Symbol_directory.name_of_id directory (Symbol_id.of_int i) in
    print_s [%message "" ~id:(i : int) ~name:(name : Symbol.t option)]);
  [%expect
    {|
    ((id 0) (name (AAPL)))
    ((id 2) (name (GOOG)))
    ((id 9) (name ()))
    |}]
;;

let%expect_test "label falls back to the id for an unknown symbol" =
  (* A known id renders as its name; an id the directory doesn't know (a
     stale mirror, say) degrades to the integer rather than raising. *)
  List.iter [ 0; 1; 42 ] ~f:(fun i ->
    print_endline (Symbol_directory.label directory (Symbol_id.of_int i)));
  [%expect {|
    AAPL
    TSLA
    42
    |}]
;;

let%expect_test "to_alist and ids" =
  print_s
    [%sexp
      (Symbol_directory.to_alist directory : (Symbol.t * Symbol_id.t) list)];
  [%expect {| ((AAPL 0) (TSLA 1) (GOOG 2)) |}];
  print_s [%sexp (Symbol_directory.ids directory : Symbol_id.t list)];
  [%expect {| (0 1 2) |}]
;;

let%expect_test "of_ids names each id by its integer" =
  let directory =
    Symbol_directory.of_ids [ Symbol_id.of_int 0; Symbol_id.of_int 1 ]
  in
  print_s
    [%sexp
      (Symbol_directory.to_alist directory : (Symbol.t * Symbol_id.t) list)];
  (* The name is the integer as a string, so a nameless run still round-trips
     and [label] shows the id. *)
  [%expect {| ((0 0) (1 1)) |}];
  print_endline (Symbol_directory.label directory (Symbol_id.of_int 1));
  [%expect {| 1 |}]
;;
