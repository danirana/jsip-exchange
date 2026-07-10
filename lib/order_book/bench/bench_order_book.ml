(** Benchmarks for the order book and matching engine.

    Run with: dune exec lib/order_book/bench/bench_order_book.exe -- existing
    -ascii -quota 5

    (The benchmarks live under named subcommands now; [existing] holds the
    original suite. Run with no subcommand to list them.)

    These benchmarks measure the core operations of the exchange and are
    designed to give you meaningful feedback on the performance of the system
    and the effect of any optimizations you make.

    {2 How to read the results}

    Core_bench reports time per operation in nanoseconds. Lower is better.
    Focus on:
    - [find_match]: the hot path — called on every incoming order
    - [submit_ioc_cross]: end-to-end order submission with a fill
    - [add/remove]: book mutation performance
    - [best_price]: how fast you can query the BBO

    {2 Tips for meaningful benchmarks}

    {ul
     {- Use [-quota 5] or higher for stable results (5 seconds per bench). }
     {- Run on a quiet machine (no heavy background processes). }
     {- Compare before/after by saving results:

       {v
          dune exec lib/order_book/bench/bench_order_book.exe -- existing -ascii -quota 5 > before.txt
          # ... make your changes ...
          dune exec lib/order_book/bench/bench_order_book.exe -- existing -ascii -quota 5 > after.txt
          diff before.txt after.txt
       v}
    }
    } *)

open! Core
open Core_bench
open Jsip_types
open Jsip_order_book

(* ---------------------------------------------------------------- *)
(* Setup helpers *)
(* ---------------------------------------------------------------- *)

let aapl = Symbol_id.of_int 0
let alice = Participant.of_string "Alice"
let bob = Participant.of_string "Bob"

(** Build a book with [n] resting sell orders at prices 1..n (in cents). This
    gives a realistic spread of prices for benchmarking find_match and
    best_price queries. *)
let book_with_n_asks ?(min_price = 10_000) n =
  let book = Order_book.create aapl in
  let gen = Order_id.Generator.create () in
  for i = 1 to n do
    let order =
      Order.create
        { symbol = aapl
        ; participant = bob
        ; side = Sell
        ; price = Price.of_int_cents (min_price + i)
        ; size = Size.of_int 100
        ; time_in_force = Day
        ; client_order_id = Client_order_id.of_int 1
        }
        ~order_id:(Order_id.Generator.next gen)
    in
    Order_book.add book order
  done;
  book, gen
;;

(** Build a book with [n] resting sell orders all at the *same* price.
    [book_with_n_asks] spreads orders across distinct prices, so its
    [snapshot] has one order per level and aggregation does nothing. This
    stacks a single level [n] deep, so [snapshot] must fold [n] orders into
    one [Level.t] -- the case that actually exercises aggregation cost. *)
let book_with_n_asks_same_price ?(price = 15_000) n =
  let book = Order_book.create aapl in
  let gen = Order_id.Generator.create () in
  for _ = 1 to n do
    Order_book.add
      book
      (Order.create
         { symbol = aapl
         ; participant = bob
         ; side = Sell
         ; price = Price.of_int_cents price
         ; size = Size.of_int 100
         ; time_in_force = Day
         ; client_order_id = Client_order_id.of_int 1
         }
         ~order_id:(Order_id.Generator.next gen))
  done;
  book
;;

(** Build a matching engine with [n] resting sells on AAPL. *)
let engine_with_n_asks ?(min_price = 10_000) n =
  let engine = Matching_engine.create [ aapl ] in
  for i = 1 to n do
    ignore
      (Matching_engine.submit
         engine
         { symbol = aapl
         ; participant = bob
         ; side = Sell
         ; price = Price.of_int_cents (min_price + i)
         ; size = Size.of_int 100
         ; time_in_force = Day
         ; client_order_id = Client_order_id.of_int 1
         }
       : Exchange_event.t list)
  done;
  engine
;;

(** Build a matching engine trading [n] distinct symbols (SYM0..SYM[n-1]) and
    return it alongside a probe symbol from the middle of the set. Exercises
    the symbol->book lookup that [book]/[submit]/[cancel] all pay but that
    the single-symbol benchmarks above never stress. *)
let engine_with_n_symbols n =
  let symbols = List.init n ~f:(fun i -> Symbol_id.of_int i) in
  let engine = Matching_engine.create symbols in
  let mid = n / 2 in
  let probe = Symbol_id.of_int mid in
  engine, probe
;;

(* ---------------------------------------------------------------- *)
(* Order_book micro-benchmarks *)
(* ---------------------------------------------------------------- *)

let bench_find_match ~n =
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  (* Incoming buy at a price that matches the best ask *)
  let incoming =
    Order.create
      { symbol = aapl
      ; participant = alice
      ; side = Buy
      ; price = Price.of_int_cents (min_price + n)
      ; size = Size.of_int 100
      ; time_in_force = Ioc
      ; client_order_id = Client_order_id.of_int 1
      }
      ~order_id:(Order_id.Generator.next gen)
  in
  Bench.Test.create ~name:[%string "find_match (n=%{n#Int})"] (fun () ->
    ignore (Order_book.find_match book incoming : Order.t option))
;;

let bench_find_match_no_cross ~n =
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  (* Incoming buy at a price below all asks — no match possible *)
  let incoming =
    Order.create
      { symbol = aapl
      ; participant = alice
      ; side = Buy
      ; price = Price.of_int_cents (min_price - 1)
      ; size = Size.of_int 100
      ; time_in_force = Ioc
      ; client_order_id = Client_order_id.of_int 1
      }
      ~order_id:(Order_id.Generator.next gen)
  in
  Bench.Test.create ~name:[%string "find_match_miss (n=%{n#Int})"] (fun () ->
    ignore (Order_book.find_match book incoming : Order.t option))
;;

let bench_best_bid_offer ~n =
  let book, _gen = book_with_n_asks n in
  Bench.Test.create ~name:[%string "best_bid_offer (n=%{n#Int})"] (fun () ->
    ignore (Order_book.best_bid_offer book : Bbo.t))
;;

let bench_add_remove ~n =
  (* Pre-build the book, then measure add+remove cycle *)
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  let order =
    Order.create
      { symbol = aapl
      ; participant = alice
      ; side = Sell
      ; price = Price.of_int_cents (min_price + 500)
      ; size = Size.of_int 100
      ; time_in_force = Day
      ; client_order_id = Client_order_id.of_int 1
      }
      ~order_id:(Order_id.Generator.next gen)
  in
  let oid = Order.order_id order in
  Bench.Test.create ~name:[%string "add+remove (n=%{n#Int})"] (fun () ->
    Order_book.add book order;
    Order_book.remove book oid)
;;

(* ---------------------------------------------------------------- *)
(* Matching engine end-to-end benchmarks *)
(* ---------------------------------------------------------------- *)

let bench_submit_ioc_cross ~n =
  (* Measure submitting an IOC order that crosses the best ask. This is the
     most common hot path: order in, fill out. We re-seed a resting order
     after each iteration to keep the book state consistent. *)
  let min_price = 10_000 in
  let max_price = 20_000 in
  let engine = engine_with_n_asks ~min_price n in
  let next_price = ref (min_price + 1) in
  Bench.Test.create
    ~name:[%string "submit_ioc_cross (n=%{n#Int})"]
    (fun () ->
       let events =
         Matching_engine.submit
           engine
           { symbol = aapl
           ; participant = alice
           ; side = Buy
           ; price = Price.of_int_cents max_price
           ; size = Size.of_int 100
           ; time_in_force = Ioc
           ; client_order_id = Client_order_id.of_int 1
           }
       in
       ignore (events : Exchange_event.t list);
       (* Re-seed: add back a resting sell to replace the one we consumed *)
       ignore
         (Matching_engine.submit
            engine
            { symbol = aapl
            ; participant = bob
            ; side = Sell
            ; price = Price.of_int_cents !next_price
            ; size = Size.of_int 100
            ; time_in_force = Day
            ; client_order_id = Client_order_id.of_int 1
            }
          : Exchange_event.t list);
       next_price := !next_price + 1;
       if !next_price > max_price then next_price := min_price + 1)
;;

let bench_submit_ioc_no_match ~n =
  let min_price = 10_000 in
  let engine = engine_with_n_asks ~min_price n in
  Bench.Test.create ~name:[%string "submit_ioc_miss (n=%{n#Int})"] (fun () ->
    ignore
      (Matching_engine.submit
         engine
         { symbol = aapl
         ; participant = alice
         ; side = Buy
         ; price = Price.of_int_cents (min_price - 1)
         ; size = Size.of_int 100
         ; time_in_force = Ioc
         ; client_order_id = Client_order_id.of_int 1
         }
       : Exchange_event.t list))
;;

let bench_submit_sweep ~n =
  (* Measure an aggressive order that sweeps through the entire book.
     Re-seeds the book after each sweep. This is worst-case: every resting
     order is visited and filled. *)
  let engine = ref (engine_with_n_asks n) in
  Bench.Test.create ~name:[%string "submit_sweep_%{n#Int}_levels"] (fun () ->
    ignore
      (Matching_engine.submit
         !engine
         { symbol = aapl
         ; participant = alice
         ; side = Buy
         ; price = Price.of_int_cents 99_999
         ; size = Size.of_int (n * 100)
         ; time_in_force = Ioc
         ; client_order_id = Client_order_id.of_int 1
         }
       : Exchange_event.t list);
    (* Re-seed entire book *)
    engine := engine_with_n_asks n)
;;

(* ---------------------------------------------------------------- *)
(* Allocation measurement *)
(* ---------------------------------------------------------------- *)

let bench_find_match_alloc ~n =
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  let incoming =
    Order.create
      { symbol = aapl
      ; participant = alice
      ; side = Buy
      ; price = Price.of_int_cents (min_price + n)
      ; size = Size.of_int 100
      ; time_in_force = Ioc
      ; client_order_id = Client_order_id.of_int 1
      }
      ~order_id:(Order_id.Generator.next gen)
  in
  (* Measure minor-heap allocations *)
  let measure_alloc f =
    Gc.compact ();
    let before = (Gc.stat ()).minor_words in
    for _ = 1 to 1000 do
      f ()
    done;
    let after = (Gc.stat ()).minor_words in
    (after -. before) /. 1000.0
  in
  let words_per_call =
    measure_alloc (fun () ->
      ignore (Order_book.find_match book incoming : Order.t option))
  in
  Bench.Test.create
    ~name:
      (sprintf "find_match_alloc (n=%d, %.1f words/call)" n words_per_call)
    (fun () -> ignore (Order_book.find_match book incoming : Order.t option))
;;

(* ---------------------------------------------------------------- *)
(* Snapshot *)
(* ---------------------------------------------------------------- *)

(** Time [snapshot] on a book whose [n] orders all rest at one price, so the
    timed work is aggregating [n] orders into a single [Level.t] -- the cost
    [book_with_n_asks] (one order per level) never surfaces. *)
let bench_snapshot ~n =
  let book = book_with_n_asks_same_price n in
  Bench.Test.create
    ~name:[%string "snapshot_same_price (n=%{n#Int})"]
    (fun () -> ignore (Order_book.snapshot book : Book.t))
;;

(* ---------------------------------------------------------------- *)
(* Symbol lookup *)
(* ---------------------------------------------------------------- *)

(** Time [book], the pure symbol->book lookup, on an engine trading [n]
    symbols. This is the site the symbol-interning optimization targets: with
    a [Symbol.Map] it is O(log n) string comparisons; the win is meant to
    show up here (and grow with [n]), without the matching work that [submit]
    and [cancel] would bury it under. *)
let bench_book_lookup ~n =
  let engine, probe = engine_with_n_symbols n in
  Bench.Test.create ~name:[%string "book_lookup (n=%{n#Int})"] (fun () ->
    ignore (Matching_engine.book engine probe : Order_book.t option))
;;

(* ---------------------------------------------------------------- *)
(* Main *)
(* ---------------------------------------------------------------- *)

let () =
  let sizes = [ 10; 50; 100; 500 ] in
  let tests =
    List.concat
      [ (* Order book micro-benchmarks at various sizes *)
        List.map sizes ~f:(fun n -> bench_find_match ~n)
      ; List.map sizes ~f:(fun n -> bench_find_match_no_cross ~n)
      ; List.map sizes ~f:(fun n -> bench_best_bid_offer ~n)
      ; [ bench_add_remove ~n:100 ]
      ; (* Matching engine end-to-end *)
        List.map sizes ~f:(fun n -> bench_submit_ioc_cross ~n)
      ; List.map sizes ~f:(fun n -> bench_submit_ioc_no_match ~n)
      ; List.map [ 10; 50; 100 ] ~f:(fun n -> bench_submit_sweep ~n)
      ; (* Allocation awareness *)
        [ bench_find_match_alloc ~n:100 ]
      ]
  in
  Command_unix.run
    (Command.group
       ~summary:"JSIP order-book benchmarks"
       [ "existing", Bench.make_command tests
       ; ( "snapshot"
         , Bench.make_command
             (List.map sizes ~f:(fun n -> bench_snapshot ~n)) )
       ; ( "book-lookup"
         , Bench.make_command
             (List.map [ 10; 100; 1_000; 10_000 ] ~f:(fun n ->
                bench_book_lookup ~n)) )
       ])
;;
