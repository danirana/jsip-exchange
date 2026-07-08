(** The dashboard's pure projection from the server's rolling window to the
    exact numbers each pane renders.

    Structured like {!Jsip_monitor.Controller}: all the interesting logic
    lives here as plain data transforms, so it is testable without any Bonsai
    machinery. The Bonsai layer only polls for a
    {!Jsip_dashboard_protocol.Window.t}, calls {!of_window}, and draws the
    resulting {!Display.t}. *)

open! Core
open Jsip_types
module Window = Jsip_dashboard_protocol.Window

(** A merged latency distribution over the whole window: percentiles plus the
    summed histogram buckets for a bar chart. Percentiles are reported as the
    bucket's exclusive upper edge ([None] = overflow / empty). *)
module Latency_summary : sig
  module Bucket : sig
    type t =
      { upper_bound : Time_ns.Span.t option
      ; count : int
      }
    [@@deriving sexp_of]
  end

  type t =
    { count : int
    ; p50 : Time_ns.Span.t option
    ; p90 : Time_ns.Span.t option
    ; p99 : Time_ns.Span.t option
    ; buckets : Bucket.t list
    }
  [@@deriving sexp_of]
end

(** Everything a single frame of the dashboard needs, decoupled from Bonsai. *)
module Display : sig
  type t =
    { memory_live_words : int list
    ; latest_live_words : int
    ; latest_heap_words : int
    ; latest_top_heap_words : int
    ; submit : Latency_summary.t
    ; cancel : Latency_summary.t
    ; book_depth : Exchange_stats.Book_depth.t list
    ; pipe_occupancy : Exchange_stats.Pipe_occupancy.t option
    ; pipe_occupancy_series : int list
    (** worst outbound-pipe depth per second, oldest to newest — the pipe
        sparkline. A climbing line means something is backing up. *)
    ; participants : Exchange_stats.Participant_activity.t list
    ; engine : Exchange_stats.Engine_busyness.t option
    ; engine_falling_behind : bool
    (** whether the matching engine is saturated, from the latest snapshot *)
    ; sample_count : int
    (** seconds the window currently spans (caps at capacity) *)
    ; uptime_seconds : int
    (** unbounded seconds since the session connected; drives the status-bar
        clock that runs past the window *)
    }
  [@@deriving sexp_of]
end

(** Merge and summarize the latency histograms of a list of snapshots.
    Exposed for testing; {!of_window} uses it per side. *)
val summarize : Exchange_stats.Latency_histogram.t list -> Latency_summary.t

(** The matching-engine saturation policy behind the engine pane's alert:
    [true] when the engine can't keep up. Fires on a real request backlog
    ([queue_depth] over a small threshold) or on submit-latency saturation
    (median [~submit] latency past ~100ms), and ignores the ambiguous
    inter-iteration gap. Shared by the dashboard and the [verify_stats]
    observer, and exposed here so it can be unit-tested directly. *)
val is_falling_behind
  :  submit:Latency_summary.t
  -> Exchange_stats.Engine_busyness.t
  -> bool

(** Project the rolling window into a renderable {!Display.t}. *)
val of_window : Window.t -> Display.t
