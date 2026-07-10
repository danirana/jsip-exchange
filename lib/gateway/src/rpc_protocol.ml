open! Core
open! Async
open Jsip_types

let login_rpc =
  Rpc.Rpc.create
    ~name:"login-rpc"
    ~version:1
    ~bin_query:String.bin_t
    ~bin_response:[%bin_type_class: Participant.t Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let session_feed_rpc =
  Rpc.Pipe_rpc.create
    ~name:"session-feed"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Exchange_event.bin_t
    ~bin_error:Error.bin_t
    ()
;;

let cancel_order_rpc =
  Rpc.Rpc.create
    ~name:"cancel-order"
    ~version:1
    ~bin_query:Client_order_id.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

(* Kill switch: cancel every order the calling session's participant has
   resting. A deliberate "flatten me" — used when a bot is stopped so it
   leaves no footprint in the book. Query is [unit]: you can only flatten
   yourself, the session you're logged in on. *)
let cancel_all_orders_rpc =
  Rpc.Rpc.create
    ~name:"cancel-all-orders"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

(* Whole-exchange kill switch: cancel every resting order across every
   participant — an operator "reset the book" action, not tied to any one
   session, so it needs no login. Used by the dashboard's "reset exchange". *)
let reset_exchange_rpc =
  Rpc.Rpc.create
    ~name:"reset-exchange"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let submit_order_rpc =
  Rpc.Rpc.create
    ~name:"submit-order"
    ~version:1
    ~bin_query:Order.Request.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let book_query_rpc =
  Rpc.Rpc.create
    ~name:"book-query"
    ~version:1
    ~bin_query:Symbol_id.bin_t
    ~bin_response:[%bin_type_class: Book.t option]
    ~include_in_error_count:Only_on_exn
;;

(* The symbol directory: every (name, id) pair the exchange trades. The wire
   still carries ids; a client fetches this once at connect so it can show
   and accept names while ids travel on every other message. *)
let symbol_directory_rpc =
  Rpc.Rpc.create
    ~name:"symbol-directory"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: (Symbol.t * Symbol_id.t) list]
    ~include_in_error_count:Only_on_exn
;;

let market_data_rpc =
  Rpc.Pipe_rpc.create
    ~name:"market-data"
    ~version:1
    ~bin_query:[%bin_type_class: Symbol_id.t list]
    ~bin_response:Exchange_event.bin_t
    ~bin_error:Error.bin_t
    ()
;;

let audit_log_rpc =
  Rpc.Pipe_rpc.create
    ~name:"audit-log"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Exchange_event.bin_t
    ~bin_error:Error.bin_t
    ()
;;

let stats_rpc =
  Rpc.Pipe_rpc.create
    ~name:"stats"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Exchange_stats.bin_t
    ~bin_error:Error.bin_t
    ()
;;
