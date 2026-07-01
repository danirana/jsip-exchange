open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
module Table = Hashtbl.Make (T)

let of_int i = i
let to_int t = t
