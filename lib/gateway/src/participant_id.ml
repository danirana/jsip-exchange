open! Core

module T = struct
  type t = int [@@deriving sexp_of, compare, equal, hash]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

module Generator = struct
  type t = { mutable next : int } [@@deriving sexp_of]

  (* 0-based so an id doubles as the dense index into the registry's [by_id]
     array. *)
  let create () = { next = 0 }

  let next t =
    let id = t.next in
    t.next <- t.next + 1;
    id
  ;;
end

module For_testing = struct
  let to_int t = t
  let of_int t = t
end
