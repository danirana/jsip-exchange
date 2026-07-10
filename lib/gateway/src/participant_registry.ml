open! Core
open Jsip_types

type t =
  { by_name : Participant_id.t Participant.Table.t
  ; by_id : Participant.t Dynarray.t
      (* Dense: index [(id :> int)] holds the name minted for that id. Grows
         by one per new intern, in lockstep with [generator], so it is total
         over every id this registry has ever handed out. *)
  ; generator : Participant_id.Generator.t
  }

let create () =
  { by_name = Participant.Table.create ()
  ; by_id = Dynarray.create ()
  ; generator = Participant_id.Generator.create ()
  }
;;

let intern t name =
  match Hashtbl.find t.by_name name with
  | Some id -> id
  | None ->
    let id = Participant_id.Generator.next t.generator in
    Hashtbl.set t.by_name ~key:name ~data:id;
    (* [id] is the next dense index, so this append lands at [(id :> int)]. *)
    Dynarray.add_last t.by_id name;
    id
;;

let id_of_name t name = Hashtbl.find t.by_name name
let name_of_id t id = Dynarray.get t.by_id (id : Participant_id.t :> int)
