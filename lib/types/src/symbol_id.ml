open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let of_int t = t
let to_int t = t
let to_string t = Int.to_string t
