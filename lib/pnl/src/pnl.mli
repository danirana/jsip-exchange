(** Per-participant, per-symbol profit-and-loss (P&L) tracking.

    A [Pnl.t] is an immutable accumulator: fold the exchange's event stream
    through {!apply_fill} and {!apply_trade_report}, then read a snapshot out
    with {!summary}.

    For each (participant, symbol) pair we track:
    - the signed [position] (positive = long, negative = short);
    - the {e average entry price} of that open position — its running cost
      basis, rolled forward on every add and left untouched by closes;
    - [realized] cash locked in whenever a trade closes part of a position.

    Unrealized P&L is marked against a per-symbol {e reference price}: the
    last public trade print, refreshed by {!apply_trade_report}. Until a
    print is seen for a symbol its unrealized P&L is zero (there is nothing
    to mark against). Note that {!apply_fill} does {b not} move the reference
    price — a fill and its public print are separate events, and P&L only
    marks against the print.

    Example:
    {[
      let pnl =
        Pnl.empty
        |> fun t ->
        Pnl.apply_fill t fill
        |> fun t -> Pnl.apply_trade_report t { symbol = aapl; price }
      in
      print_endline (Pnl.Summary.to_string_hum (Pnl.summary pnl alice))
    ]} *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** A P&L accumulator with no positions and no reference prices. *)
val empty : t

(** A public trade print, carrying only what P&L needs to mark positions: the
    symbol and the price it printed at. This mirrors the [Trade_report]
    constructor of {!Jsip_types.Exchange_event.t} (which additionally carries
    the traded size — irrelevant here). *)
module Trade_report : sig
  type t =
    { symbol : Symbol_id.t
    ; price : Price.t
    }
  [@@deriving sexp, bin_io]
end

(** Apply a fill to {e both} of its participants. The aggressor trades on
    [aggressor_side]; the resting participant trades the opposite side at the
    same price and size. Each side's position, average entry price, and
    realized cash are updated. Reference prices are left unchanged. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference price used to mark unrealized P&L for the report's
    symbol. Existing positions in that symbol are re-marked the next time
    {!summary} is called. *)
val apply_trade_report : t -> Trade_report.t -> t

module Summary : sig
  (** One symbol's contribution to a participant's P&L. *)
  module Per_symbol : sig
    type t =
      { symbol : Symbol_id.t
      ; position : int
      (** Signed share count; positive long, negative short. *)
      ; average_entry_price : Price.t option
      (** The open position's average entry price, or [None] when flat. *)
      ; reference_price : Price.t option
      (** The last trade print seen, or [None] if none has been. *)
      ; realized_cents : int (** Cash from closed positions, in cents. *)
      ; unrealized_cents : int
      (** [position * (reference_price - average_entry_price)], in cents; 0
          when there is no reference price. *)
      }
    [@@deriving sexp_of]
  end

  type t =
    { per_symbol : Per_symbol.t list
    ; realized_cents : int (** Total realized across all symbols. *)
    ; unrealized_cents : int (** Total unrealized across all symbols. *)
    }
  [@@deriving sexp_of]

  (** Net P&L: [realized_cents + unrealized_cents]. *)
  val total_cents : t -> int

  (** A compact, human-readable multi-line breakdown with dollar formatting.
      Handy for expect tests and monitor output. *)
  val to_string_hum : t -> string
end

(** The per-symbol breakdown and totals for one participant. Symbols the
    participant has never traded are omitted; an unknown participant yields
    an empty summary. *)
val summary : t -> Participant.t -> Summary.t
