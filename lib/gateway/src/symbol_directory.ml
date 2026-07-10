open! Core
open Jsip_types

type t =
  { by_name : Symbol_id.t Symbol.Map.t
  ; by_id : Symbol.t Symbol_id.Map.t
  }

let create pairs =
  { by_name = Symbol.Map.of_alist_exn pairs
  ; by_id =
      Symbol_id.Map.of_alist_exn
        (List.map pairs ~f:(fun (name, id) -> id, name))
  }
;;

let of_ids ids =
  create
    (List.map ids ~f:(fun id ->
       Symbol.of_string (Symbol_id.to_string id), id))
;;

let id_of_name t name = Map.find t.by_name name
let name_of_id t id = Map.find t.by_id id

let label t id =
  match name_of_id t id with
  | Some name -> Symbol.to_string name
  | None -> Symbol_id.to_string id
;;

(* Ordered by id (0, 1, 2, …), which reads naturally and is stable across
   runs. Order is not load-bearing — a consumer rebuilds a map from this. *)
let to_alist t =
  Map.to_alist t.by_id |> List.map ~f:(fun (id, name) -> name, id)
;;

let ids t = Map.keys t.by_id
