open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Jsip_types
module Controller = Jsip_dashboard_controller.Controller
module Protocol = Jsip_dashboard_protocol

let poll_every = Time_ns.Span.of_sec 1.

(* A compact clock for the uptime readout: [45s], [4m 12s], [1h 03m]. Unlike
   the window length it is never capped, so it keeps climbing past 60s. *)
let format_uptime total_seconds =
  let hours = total_seconds / 3600 in
  let minutes = total_seconds / 60 % 60 in
  let seconds = total_seconds % 60 in
  if hours > 0
  then [%string "%{hours#Int}h %{minutes#Int}m"]
  else if minutes > 0
  then [%string "%{minutes#Int}m %{seconds#Int}s"]
  else [%string "%{seconds#Int}s"]
;;

module Launcher = struct
  (* The control that turns this read-only monitor into a control plane: pick
     a bot, click launch, and the dashboard server starts it against the
     exchange. The last launch's result shows beside the button — green on
     success, red on failure (e.g. no market data yet). *)
  (* Classify a dispatcher outcome into (message, is_ok). The outer [Error]
     is a connection/dispatch failure; the inner [Ok/Error] is the server's
     own answer. *)
  let outcome_message ~ok = function
    | Error error ->
      [%string "couldn't reach server: %{Error.to_string_hum error}"], false
    | Ok (Error error) -> Error.to_string_hum error, false
    | Ok (Ok ()) -> ok, true
  ;;

  let component (local_ graph) : Vdom.Node.t Bonsai.t =
    let launch =
      Rpc_effect.Rpc.dispatcher Protocol.Rpcs.launch_bot_rpc graph
    in
    let stop = Rpc_effect.Rpc.dispatcher Protocol.Rpcs.stop_bot_rpc graph in
    let reset =
      Rpc_effect.Rpc.dispatcher Protocol.Rpcs.reset_bots_rpc graph
    in
    let reset_exchange =
      Rpc_effect.Rpc.dispatcher Protocol.Rpcs.reset_exchange_rpc graph
    in
    (* Poll the bots the server is tracking, so the stop chips stay in sync
       even across a page reload. *)
    let running =
      Rpc_effect.Rpc.poll
        Protocol.Rpcs.running_bots_rpc
        ~equal_query:[%equal: unit]
        ~every:(Bonsai.return poll_every)
        ~output_type:Last_ok_response
        (Bonsai.return ())
        graph
    in
    let selected, set_selected =
      Bonsai.state (List.hd_exn Protocol.Bot_kind.all) graph
    in
    (* [Some (message, is_ok)] once an action has resolved; [None] before
       any. *)
    let result, set_result =
      Bonsai.state (None : (string * bool) option) graph
    in
    let%arr launch
    and stop
    and reset
    and reset_exchange
    and running
    and selected
    and set_selected
    and result
    and set_result in
    let running = Option.value running ~default:[] in
    let on_change =
      Vdom.Attr.on_change (fun _ value ->
        match Protocol.Bot_kind.of_value_string value with
        | Some kind -> set_selected kind
        | None -> Effect.return ())
    in
    (* Effects build at click time (not every stabilization), so the dispatch
       fires once, when clicked. *)
    let on_launch _ =
      let%bind.Effect outcome = launch selected in
      set_result
        (Some
           (outcome_message
              outcome
              ~ok:
                [%string
                  "launched %{Protocol.Bot_kind.to_display_string selected}"]))
    in
    let on_reset _ =
      let%bind.Effect outcome = reset () in
      set_result
        (Some (outcome_message outcome ~ok:"reset — all bots stopped"))
    in
    let on_reset_exchange _ =
      let%bind.Effect outcome = reset_exchange () in
      set_result
        (Some (outcome_message outcome ~ok:"exchange reset — book cleared"))
    in
    let options =
      List.map Protocol.Bot_kind.all ~f:(fun kind ->
        let value = Protocol.Bot_kind.to_value_string kind in
        let selected_attr =
          if Protocol.Bot_kind.equal kind selected
          then [ Vdom.Attr.create "selected" "" ]
          else []
        in
        {%html|
          <option value=%{value} *{selected_attr}>
            #{Protocol.Bot_kind.to_display_string kind}
          </option>
        |})
    in
    (* One chip per running bot, each with an ✕ that stops just that bot. *)
    let chips =
      List.map running ~f:(fun (bot : Protocol.Running_bot.t) ->
        let on_stop _ =
          let%bind.Effect outcome = stop bot.participant in
          set_result
            (Some
               (outcome_message
                  outcome
                  ~ok:[%string "stopped %{bot.participant#Participant}"]))
        in
        {%html|
          <span %{Styles.attr Styles.running_chip}>
            #{Participant.to_string bot.participant}
            <button
              %{Styles.attr Styles.running_chip_stop}
              on_click=%{on_stop}>✕</button>
          </span>
        |})
    in
    let running_node =
      match running with
      | [] -> {%html|<span></span>|}
      | _ :: _ ->
        {%html|
          <>
            <div %{Styles.attr Styles.running_area}>*{chips}</div>
            <button
              %{Styles.attr Styles.reset_button}
              on_click=%{on_reset}>reset</button>
          </>
        |}
    in
    let note =
      match result with
      | None -> {%html|<span></span>|}
      | Some (message, is_ok) ->
        let style = if is_ok then Styles.launch_ok else Styles.launch_err in
        {%html|<span %{Styles.attr style}>#{message}</span>|}
    in
    {%html|
      <div %{Styles.attr Styles.launcher}>
        <select %{Styles.attr Styles.launcher_select} %{on_change}>
          *{options}
        </select>
        <button
          %{Styles.attr Styles.launcher_button}
          on_click=%{on_launch}>launch bot</button>
        %{running_node}
        <button
          %{Styles.attr Styles.reset_button}
          on_click=%{on_reset_exchange}>reset exchange</button>
        %{note}
      </div>
    |}
  ;;
end

(* The top band: brand on the left, the bot launcher, a window readout, and a
   live indicator pushed to the right. [live_color] tints both the dot and
   its label, so the connecting state reuses the same cluster in warn. *)
let status_bar ~live_color ~live_text ~window_note ~launcher =
  {%html|
    <div %{Styles.attr Styles.statusbar}>
      <div %{Styles.attr Styles.brand}>
        <span %{Styles.attr Styles.brand_mark}></span>
        <span>JSIP</span>
        <span %{Styles.attr Styles.brand_sub}>exchange terminal</span>
      </div>
      <span %{Styles.attr Styles.statusbar_spacer}></span>
      %{launcher}
      <span %{Styles.attr Styles.status_item}>#{window_note}</span>
      <div %{Styles.attr Styles.live_group}>
        <span %{Styles.attr (Styles.live_dot live_color)}></span>
        <span %{Styles.attr (Styles.live_label live_color)}>#{live_text}</span>
      </div>
    </div>
  |}
;;

(* Market first (the book is the hero, participants ride alongside), then the
   infrastructure health panes in a dense grid below. *)
let render ~launcher (display : Controller.Display.t) =
  {%html|
    <div %{Styles.attr Styles.page}>
      %{status_bar
          ~live_color:"var(--color-good)"
          ~live_text:"LIVE"
          ~window_note:
            [%string
              "uptime %{format_uptime display.uptime_seconds} · 1 update/s"]
          ~launcher}
      <div %{Styles.attr Styles.main}>
        <span %{Styles.attr Styles.section_label}>market</span>
        <div %{Styles.attr Styles.hero_row}>
          <div %{Styles.attr Styles.hero_main}>
            %{Panes.Book.view ~rows:display.book_depth}
          </div>
          <div %{Styles.attr Styles.hero_side}>
            %{Panes.Participants.view ~rows:display.participants}
          </div>
        </div>
        <span %{Styles.attr Styles.section_label}>infrastructure</span>
        <div %{Styles.attr Styles.grid}>
          %{Panes.Memory.view
              ~series:display.memory_live_words
              ~latest_words:display.latest_live_words
              ~top_words:display.latest_top_heap_words}
          %{Panes.Latency.view ~title:"submit latency" display.submit}
          %{Panes.Latency.view ~title:"cancel latency" display.cancel}
          %{Panes.Engine.view
              ~falling_behind:display.engine_falling_behind
              display.engine}
          %{Panes.Pipe_occupancy.view
              ~series:display.pipe_occupancy_series
              display.pipe_occupancy}
        </div>
      </div>
    </div>
  |}
;;

(* Shown until the first window arrives: the same chrome, a warn-tinted
   indicator, and a quiet placeholder where the market will land. *)
let connecting ~launcher =
  {%html|
    <div %{Styles.attr Styles.page}>
      %{status_bar
          ~live_color:"var(--color-warn)"
          ~live_text:"CONNECTING"
          ~window_note:"waiting for exchange…"
          ~launcher}
      <div %{Styles.attr Styles.main}>
        <span %{Styles.attr Styles.empty}>connecting to exchange…</span>
      </div>
    </div>
  |}
;;

(* Poll the dashboard server once per second for the whole rolling window,
   then project and render it. [~where_to_connect] is omitted, so the client
   opens a websocket back to the host that served the page — the dashboard
   server. A backgrounded tab simply stops polling; nothing backs up. *)
let app (local_ graph) : Vdom.Node.t Bonsai.t =
  let window =
    Rpc_effect.Rpc.poll
      Protocol.Rpcs.recent_stats_rpc
      ~equal_query:[%equal: unit]
      ~every:(Bonsai.return poll_every)
      ~output_type:Last_ok_response
      (Bonsai.return ())
      graph
  in
  let launcher = Launcher.component graph in
  let%arr window and launcher in
  match window with
  | None -> connecting ~launcher
  | Some window -> render ~launcher (Controller.of_window window)
;;
