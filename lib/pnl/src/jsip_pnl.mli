(** Per-participant profit-and-loss tracking for the JSIP exchange.

    Exposes {!Pnl}, an immutable accumulator that folds fills and trade
    reports into per-participant, per-symbol positions, realized cash, and
    unrealized marks. Bots, the monitor, and end-of-day reporting can all
    read snapshots from it without touching the matching engine. *)

module Pnl = Pnl
