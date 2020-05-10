open Tsdl

module LocalGame = Game.Make (State.Local)

(** [main] waits for a game start, then exectues that game. *)
let main () =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init false "./resources/"  in
    Audio.adjust_music audio 0.05; 
    Menu.init audio graphics
      [("Multiplayer", "checkbox", 
        (fun m -> Menu_state.toggle_multiplayer m )); 
       ("Start", "checkbox", (fun m -> print_endline "start clicked";  m))];
    Sdl.quit ();
    exit 0

let () = main ()
