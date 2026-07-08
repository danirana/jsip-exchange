open! Core
open Jsip_types

(* The rolling window of per-second snapshots the dashboard server keeps and
   ships to the browser. Oldest first, newest last, capped at [capacity]
   entries (~60s of history). Because the client renders the whole window
   each poll, it shows a bounded time series without accumulating anything
   itself — a backgrounded tab that misses polls simply resyncs to the latest
   window when it comes back. *)

let capacity = 60

type t =
  { samples : Exchange_stats.t list
  ; total_samples : int
  }
[@@deriving sexp, bin_io]

let empty = { samples = []; total_samples = 0 }

(* Append the newest sample and drop the oldest beyond [capacity].
   O(capacity) per call, which at one snapshot per second is nothing.
   [total_samples] counts every sample ever added and is never capped, so it
   doubles as an uptime-in-seconds counter for the watching session. *)
let add t sample =
  let samples = t.samples @ [ sample ] in
  let overflow = List.length samples - capacity in
  let samples =
    if overflow > 0 then List.drop samples overflow else samples
  in
  { samples; total_samples = t.total_samples + 1 }
;;
