(** The rolling window of per-second stats snapshots the dashboard server
    maintains and streams to the browser client, oldest first. *)

open! Core
open Jsip_types

(** Maximum number of snapshots retained (~60 seconds of history). *)
val capacity : int

type t =
  { samples : Exchange_stats.t list
  ; total_samples : int
  (** Count of every snapshot seen since the watching session connected,
      never capped — unlike [List.length samples], which tops out at
      {!capacity}. At one snapshot per second it doubles as an uptime-seconds
      counter, so the client can show a clock that runs past the window. *)
  }
[@@deriving sexp, bin_io]

(** An empty window. *)
val empty : t

(** Append the newest snapshot, evicting the oldest once {!capacity} is
    exceeded. *)
val add : t -> Exchange_stats.t -> t
