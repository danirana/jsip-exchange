open! Core
open Jsip_types
open Jsip_test_harness
module Window = Jsip_dashboard_protocol.Window
module Controller = Jsip_dashboard_controller.Controller

(* Build a latency histogram straight from a bucket-count list, so tests can
   pin exactly where samples land without going through [bucket_index]. *)
let histogram counts : Exchange_stats.Latency_histogram.t =
  { counts
  ; total = List.sum (module Int) counts ~f:Fn.id
  ; sum = Time_ns.Span.zero
  }
;;

let empty_histogram = Exchange_stats.Latency_histogram.empty

let empty_pipe_occupancy : Exchange_stats.Pipe_occupancy.t =
  { market_data_max = 0
  ; audit_max = 0
  ; session_max = 0
  ; slowest_session = None
  }
;;

let empty_engine : Exchange_stats.Engine_busyness.t =
  { queue_depth = 0
  ; max_gap = Time_ns.Span.zero
  ; mean_gap = Time_ns.Span.zero
  }
;;

let snapshot
  ?(submit = empty_histogram)
  ?(cancel = empty_histogram)
  ?(book_depth = [])
  ?(participants = [])
  ~live_words
  ()
  : Exchange_stats.t
  =
  { live_words
  ; heap_words = live_words * 2
  ; top_heap_words = live_words * 3
  ; submit_latency = submit
  ; cancel_latency = cancel
  ; book_depth
  ; pipe_occupancy = empty_pipe_occupancy
  ; participants
  ; engine = empty_engine
  }
;;

let window_of samples = List.fold samples ~init:Window.empty ~f:Window.add

let%expect_test "empty window projects to zeros and no percentiles" =
  let display = Controller.of_window Window.empty in
  print_s [%sexp (display : Controller.Display.t)];
  [%expect
    {|
    ((memory_live_words ()) (latest_live_words 0) (latest_heap_words 0)
     (latest_top_heap_words 0)
     (submit
      ((count 0) (p50 ()) (p90 ()) (p99 ())
       (buckets
        (((upper_bound (1us)) (count 0)) ((upper_bound (10us)) (count 0))
         ((upper_bound (100us)) (count 0)) ((upper_bound (1ms)) (count 0))
         ((upper_bound (10ms)) (count 0)) ((upper_bound (100ms)) (count 0))
         ((upper_bound (1s)) (count 0)) ((upper_bound ()) (count 0))))))
     (cancel
      ((count 0) (p50 ()) (p90 ()) (p99 ())
       (buckets
        (((upper_bound (1us)) (count 0)) ((upper_bound (10us)) (count 0))
         ((upper_bound (100us)) (count 0)) ((upper_bound (1ms)) (count 0))
         ((upper_bound (10ms)) (count 0)) ((upper_bound (100ms)) (count 0))
         ((upper_bound (1s)) (count 0)) ((upper_bound ()) (count 0))))))
     (book_depth ()) (pipe_occupancy ()) (pipe_occupancy_series ())
     (participants ()) (engine ()) (engine_falling_behind false) (sample_count 0)
     (uptime_seconds 0))
    |}]
;;

let%expect_test "summarize picks the bucket the target rank lands in" =
  (* 10 samples all at bucket 2 (upper bound 100us). Every percentile is that
     bucket's upper edge. *)
  let all_in_100us = histogram [ 0; 0; 10; 0; 0; 0; 0; 0 ] in
  print_s
    [%sexp
      (Controller.summarize [ all_in_100us ] : Controller.Latency_summary.t)];
  [%expect
    {|
    ((count 10) (p50 (100us)) (p90 (100us)) (p99 (100us))
     (buckets
      (((upper_bound (1us)) (count 0)) ((upper_bound (10us)) (count 0))
       ((upper_bound (100us)) (count 10)) ((upper_bound (1ms)) (count 0))
       ((upper_bound (10ms)) (count 0)) ((upper_bound (100ms)) (count 0))
       ((upper_bound (1s)) (count 0)) ((upper_bound ()) (count 0)))))
    |}]
;;

let%expect_test "summarize merges histograms and overflow reads as None" =
  (* Split across the fastest bucket and the overflow bucket. p50 lands in
     bucket 0 (1us); p90/p99 land in overflow, reported as None. *)
  let split = histogram [ 5; 0; 0; 0; 0; 0; 0; 5 ] in
  let summary = Controller.summarize [ split ] in
  print_s
    [%sexp
      { count = (summary.count : int)
      ; p50 = (summary.p50 : Time_ns.Span.t option)
      ; p90 = (summary.p90 : Time_ns.Span.t option)
      ; p99 = (summary.p99 : Time_ns.Span.t option)
      }];
  [%expect {| ((count 10) (p50 (1us)) (p90 ()) (p99 ())) |}]
;;

let%expect_test "of_window keeps the memory series and the latest book depth"
  =
  let depth =
    [ { Exchange_stats.Book_depth.symbol = Harness.aapl
      ; bbo = Bbo.empty
      ; resting_size_bid = Size.of_int 300
      ; resting_size_ask = Size.of_int 150
      }
    ]
  in
  let window =
    window_of
      [ snapshot ~live_words:1000 ()
      ; snapshot ~live_words:1500 ()
      ; snapshot ~live_words:2000 ~book_depth:depth ()
      ]
  in
  let display = Controller.of_window window in
  print_s
    [%sexp
      { memory = (display.memory_live_words : int list)
      ; latest = (display.latest_live_words : int)
      ; samples = (display.sample_count : int)
      ; depth = (display.book_depth : Exchange_stats.Book_depth.t list)
      }];
  [%expect
    {|
    ((memory (1000 1500 2000)) (latest 2000) (samples 3)
     (depth
      (((symbol 0) (bbo ((bid ()) (ask ()))) (resting_size_bid 300)
        (resting_size_ask 150)))))
    |}]
;;

let engine ~queue_depth : Exchange_stats.Engine_busyness.t =
  { queue_depth; max_gap = Time_ns.Span.zero; mean_gap = Time_ns.Span.zero }
;;

(* [submit_counts] pins the submit histogram into buckets
   [1us;10us;100us;1ms; 10ms;100ms;1s;overflow], so [p50] lands where we
   choose. *)
let falling_behind ~queue_depth ~submit_counts =
  Controller.is_falling_behind
    ~submit:(Controller.summarize [ histogram submit_counts ])
    (engine ~queue_depth)
;;

let%expect_test "is_falling_behind: queue backlog OR submit-latency \
                 saturation"
  =
  let show label b = printf "%-22s %b\n" label b in
  (* Healthy: no backlog, median submit latency in the 1ms bucket. *)
  show
    "healthy p50=1ms"
    (falling_behind
       ~queue_depth:0
       ~submit_counts:[ 0; 0; 0; 20; 0; 0; 0; 0 ]);
  (* Bursty but not saturated: median in the 100ms bucket — the threshold is
     strict, so exactly 100ms does not trip (guards order-spam's bursts). *)
  show
    "bursty p50=100ms"
    (falling_behind
       ~queue_depth:0
       ~submit_counts:[ 0; 0; 0; 0; 0; 20; 0; 0 ]);
  (* Saturated: median in the overflow bucket (>= 1s) trips on latency alone,
     even though [queue_depth] sampled 0 — the book-fill case. *)
  show
    "saturated p50>=1s"
    (falling_behind
       ~queue_depth:0
       ~submit_counts:[ 0; 0; 0; 0; 0; 0; 0; 20 ]);
  (* Backlog trips on its own, even with fast latency. *)
  show
    "queue=8 fast"
    (falling_behind
       ~queue_depth:8
       ~submit_counts:[ 0; 0; 0; 20; 0; 0; 0; 0 ]);
  (* Just under the queue threshold with fast latency stays quiet. *)
  show
    "queue=7 fast"
    (falling_behind
       ~queue_depth:7
       ~submit_counts:[ 0; 0; 0; 20; 0; 0; 0; 0 ]);
  (* No traffic at all: not saturation, just idle. *)
  show
    "empty"
    (falling_behind ~queue_depth:0 ~submit_counts:[ 0; 0; 0; 0; 0; 0; 0; 0 ]);
  [%expect
    {|
    healthy p50=1ms        false
    bursty p50=100ms       false
    saturated p50>=1s      true
    queue=8 fast           true
    queue=7 fast           false
    empty                  false
    |}]
;;
