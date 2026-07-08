open! Core

(* The dashboard client entry point. [Bonsai_web.Start.start] mounts the app
   into the [<div id="app">] in index.html and drives stabilization. *)
let () = Bonsai_web.Start.start App.app
