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
         ("Start", "action", (fun m -> print_endline "start"; 
                               Menu_state.set_start_game m true))] in
    Sdl.quit ();
    exit 0

let () = Lwt_main.run @@ main
