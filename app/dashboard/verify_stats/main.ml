open! Core
open! Async
module Stats = Jsip_types.Exchange_stats
module Controller = Jsip_dashboard_controller.Controller

(* Throwaway observer: subscribe to the exchange stats stream and print, once
   per second, the numbers the dashboard panes render — process memory,
   submit and cancel latency percentiles, matching-engine backlog, and total
   resting book size — so we can watch how a pathological scenario moves each
   one. Percentiles reuse the dashboard's own [Controller.summarize] on a
   single-second histogram. Delete after. *)

(* One second's p50/p99 off that second's histogram alone. *)
let pctiles (h : Stats.Latency_histogram.t) =
  let s = Controller.summarize [ h ] in
  let show = function
    | None -> "  —   "
    | Some span -> Time_ns.Span.to_string span
  in
  Printf.sprintf "n=%-5d p50=%-8s p99=%-8s" s.count (show s.p50) (show s.p99)
;;

let total_resting (depth : Stats.Book_depth.t list) =
  List.sum (module Int) depth ~f:(fun d ->
    Jsip_types.Size.to_int d.resting_size_bid
    + Jsip_types.Size.to_int d.resting_size_ask)
;;

let main ~port ~ticks () =
  let%bind conn =
    Rpc.Connection.client
      (Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port })
    >>| function Ok conn -> conn | Error exn -> raise exn
  in
  let%bind pipe, _ =
    Rpc.Pipe_rpc.dispatch_exn Jsip_gateway.Rpc_protocol.stats_rpc conn ()
  in
  let n = ref 0 in
  Pipe.iter_without_pushback pipe ~f:(fun (s : Stats.t) ->
    incr n;
    let mb = Float.of_int (s.live_words * 8) /. 1_000_000. in
    let submit = Controller.summarize [ s.submit_latency ] in
    printf
      "t=%2d | mem=%7.1f MB | rest=%-8d | submit %s | cancel %s | q=%-3d \
       gap=%-8s %s\n\
       %!"
      !n
      mb
      (total_resting s.book_depth)
      (pctiles s.submit_latency)
      (pctiles s.cancel_latency)
      s.engine.queue_depth
      (Time_ns.Span.to_string s.engine.mean_gap)
      (if Controller.is_falling_behind ~submit s.engine
       then "<ENGINE ALERT>"
       else "");
    if !n >= ticks then Shutdown.shutdown 0)
;;

let () =
  Command_unix.run
    (Command.async
       ~summary:"observe stats"
       (let%map_open.Command port =
          flag "-port" (optional_with_default 12345 int) ~doc:"PORT"
        and ticks =
          flag
            "-ticks"
            (optional_with_default 12 int)
            ~doc:"N snapshots to read before exiting"
        in
        fun () -> main ~port ~ticks ()))
;;
