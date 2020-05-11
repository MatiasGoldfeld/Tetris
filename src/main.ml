open Tsdl
open Lwt.Infix
open Ppx_lwt

(** [main] loads SDL components and starts up the main menu. *)
let main : unit Lwt.t =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init false "./resources/"  in
    Audio.adjust_music audio 0.05;
    let%lwt () = Menu.init audio graphics
        [("Multiplayer", "checkbox", 
          (fun m -> Menu_state.toggle_multiplayer m )); 
         ("Host game?", "checkbox", (fun m -> Menu_state.toggle_host m));
         ("Increase Volume (by 10%)", "action", 
          (fun m ->  Menu_state.adjust_music m 0.1));
         ("Decrease Volume (by 10%)", "action",
          (fun m -> Menu_state.adjust_music m (-0.1)));
         ("Start", "action", (fun m -> Menu_state.set_start_game m true));
         ("View Leaderboard", "action", 
          (fun m -> Menu_state.toggle_leaderboard m))] in
    Sdl.quit ();
    exit 0

let () = Lwt_main.run main
