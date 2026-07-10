open! Core

module List_seq = struct
  type t = int list ref

  let create () = ref []

  let set t ~key ~data =
    match List.length !t with
    | length when key < 0 || key > length ->
      raise_s [%message "Index out of range" (key : int) (length : int)]
    | length when key = length -> t := !t @ [ data ]
    | _length ->
      t := List.mapi !t ~f:(fun i x -> if i = key then data else x)
  ;;

  let get t key = List.nth !t key
  let remove t key = t := List.filteri !t ~f:(fun i _ -> i <> key)
end

module Dynarray_seq = struct
  type t = int Dynarray.t

  let create () = Dynarray.create ()

  let set t ~key ~data =
    match Dynarray.length t with
    | length when key < 0 || key > length ->
      raise_s [%message "Index out of range" (key : int) (length : int)]
    | length when key = length -> Dynarray.add_last t data
    | _length -> Dynarray.set t key data
  ;;

  let get t key =
    if key >= 0 && key < Dynarray.length t
    then Some (Dynarray.get t key)
    else None
  ;;

  let remove t key =
    let length = Dynarray.length t in
    if key >= 0 && key < length
    then (
      for i = key to length - 2 do
        Dynarray.set t i (Dynarray.get t (i + 1))
      done;
      Dynarray.remove_last t)
  ;;
end
