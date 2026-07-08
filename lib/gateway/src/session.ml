open! Core
open! Async
open Jsip_types

type t =
  { participant : Participant.t
  ; reader : Exchange_event.t Pipe.Reader.t
  ; writer : Exchange_event.t Pipe.Writer.t
  ; budget : int
  (* Slow-consumer disconnect threshold: once this many events are queued
     unread, [push] closes the pipe instead of buffering more. *)
  }

let create ~budget participant =
  let reader, writer = Pipe.create () in
  { participant; reader; writer; budget }
;;

let participant t = t.participant
let reader t = t.reader

(* Session events (fills, cancels, accepts) are not superseded by later ones,
   so we never drop them. Instead, a participant that falls [budget] events
   behind is disconnected: close the pipe (the client's reader EOFs) and log.
   The client must reconnect and resync — honest about the loss, rather than
   silently corrupting its view of its own orders. *)
let push t event =
  if Pipe.is_closed t.writer
  then ()
  else if Pipe.length t.writer >= t.budget
  then (
    let participant = t.participant in
    let budget = t.budget in
    [%log.error
      "session feed slow consumer; disconnecting"
        (participant : Participant.t)
        (budget : int)];
    Pipe.close t.writer)
  else Pipe.write_without_pushback_if_open t.writer event
;;

let close t = Pipe.close t.writer
let is_closed t = Pipe.is_closed t.writer
let closed t = Pipe.closed t.writer
let queue_length t = Pipe.length t.writer
