(** Shared helpers for end-to-end tests that use a real server and RPC
    clients. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway

(** Start a server on an OS-assigned port, run [f], then shut down. *)
val with_server
  :  symbols:Symbol_id.t list
  -> (server:Exchange_server.t -> port:int -> 'a Deferred.t)
  -> 'a Deferred.t

(** A test client: an open RPC connection to the server, together with a
    background worker draining its [session_feed_rpc] channel. *)
type client

(** [connect_as ~port participant] connects to the running exchange server on
    [port], authenticates via [login_rpc], and subscribes to the persistent
    [session_feed_rpc] channel. Spawns a concurrent background worker that
    flushes all received execution events directly to stdout, prefixed with
    [[participant]] for seamless tracking within multi-client expect tests. *)
val connect_as : port:int -> Participant.t -> client Deferred.t

(** The raw RPC connection, useful for tests that exercise unusual RPC paths
    (audit log subscriptions, second clients on the same connection, etc.). *)
val connection : client -> Rpc.Connection.t

(** Submit an order via RPC. The RPC is one-way: this returns once the server
    has enqueued the request. Participant-targeted events (acceptance, fills,
    rejection) arrive asynchronously on this client's session feed, which
    {!connect_as} flushes to stdout prefixed with the participant. *)
val rpc_submit : client -> Order.Request.t -> unit Deferred.t

(** Query the book via RPC. *)
val rpc_book : client -> Symbol_id.t -> Book.t option Deferred.t
