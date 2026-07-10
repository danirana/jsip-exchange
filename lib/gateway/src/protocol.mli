(** Text protocol for communicating with the exchange.

    This module defines how order requests are represented as text and how
    exchange events are formatted for display. On a production exchange, this
    would be a binary protocol like FIX for performance and interoperability.
    We use a simple human-readable text format for ease of debugging and
    interactive use.

    {2 Command format}

    Each command is a single line of text:
    {v
    BUY  <symbol> <size> <price> [<time_in_force>] [as <participant>]
    SELL <symbol> <size> <price> [<time_in_force>] [as <participant>]
    v}

    Examples:
    {v
    BUY AAPL 100 150.25
    SELL TSLA 50 200.00 IOC
    BUY AAPL 100 150.00 DAY as Alice
    v}

    Time-in-force defaults to DAY if omitted. Participant defaults to
    "anonymous" if omitted. *)

open! Core
open Jsip_types

(** Format an exchange event as a single line of human-readable text.
    [render_symbol] turns a symbol id into its display string; it defaults to
    the integer, so server-side and test callers keep printing ids. A
    consumer holding a {!Symbol_directory} passes
    [Symbol_directory.label dir] to show names while the wire still carried
    the id. *)
val format_event
  :  ?render_symbol:(Symbol_id.t -> string)
  -> Exchange_event.t
  -> string

(** Format a list of events, one per line. See {!format_event} for
    [render_symbol]. *)
val format_events
  :  ?render_symbol:(Symbol_id.t -> string)
  -> Exchange_event.t list
  -> string

(** Render a book snapshot like {!Jsip_types.Book.to_string}, but resolve the
    header symbol via [render_symbol] (defaulting to the integer id). *)
val format_book : ?render_symbol:(Symbol_id.t -> string) -> Book.t -> string

(** The "You bought/sold N <symbol> at $P" line a participant sees for its
    own fill — the name-aware analogue of
    {!Jsip_types.Fill.to_participant_view}. [None] when [viewer] is neither
    side of the fill. *)
val fill_participant_view
  :  ?render_symbol:(Symbol_id.t -> string)
  -> Fill.t
  -> viewer:Participant.t
  -> string option
