open! Core

module Map_int = struct
  (* An [int]-keyed Core [Map] is immutable, so -- like [List_seq] -- we hold
     it in a [ref] and overwrite it on every [set]. *)
  type t = int Int.Map.t ref

  let create () = ref Int.Map.empty
  let set t ~key ~data = t := Map.set !t ~key ~data
  let get t key = Map.find !t key
end

module Hashtable_int = struct
  (* A Core [Hashtbl] mutates in place, so -- like [Dynarray_seq] -- no [ref]
     is needed; [set] updates [t] directly. *)
  type t = int Int.Table.t

  let create () = Int.Table.create ()
  let set t ~key ~data = Hashtbl.set t ~key ~data
  let get t key = Hashtbl.find t key
end

module Map_string = struct
  (* Same shape as [Map_int], but keyed by [string] via [String.Map]. *)
  type t = int String.Map.t ref

  let create () = ref String.Map.empty
  let set t ~key ~data = t := Map.set !t ~key ~data
  let get t key = Map.find !t key
end

module Hashtable_string = struct
  (* Same shape as [Hashtable_int], but keyed by [string] via [String.Table]. *)
  type t = int String.Table.t

  let create () = String.Table.create ()
  let set t ~key ~data = Hashtbl.set t ~key ~data
  let get t key = Hashtbl.find t key
end

module Fat_record = struct
  module T = struct
    type t =
      { a : int
      ; b : string
      ; c : float
      ; d : int
      ; e : string
      ; f : bool
      ; g : int
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_index i =
    { a = i
    ; b = Int.to_string i
    ; c = Float.of_int i
    ; d = i * 2
    ; e = sprintf "key-%d" i
    ; f = Int.equal (i land 1) 0
    ; g = i * i
    }
  ;;
end

module Map_record = struct
  (* Same shape as [Map_int]/[Map_string], keyed by the fat record via the
     derived [Fat_record.Map]; comparing a fat key is where this store pays
     its cost. *)
  type t = int Fat_record.Map.t ref

  let create () = ref Fat_record.Map.empty
  let set t ~key ~data = t := Map.set !t ~key ~data
  let get t key = Map.find !t key
end

module Hashtable_record = struct
  (* [Fat_record.Table] comes from the [Hashable.Make (T)] include in
     [Fat_record]; hashing a fat key is where this store pays its cost. *)
  type t = int Fat_record.Table.t

  let create () = Fat_record.Table.create ()
  let set t ~key ~data = Hashtbl.set t ~key ~data
  let get t key = Hashtbl.find t key
end
