open Tsdl

let main () =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init () in
    Game.init 1 audio graphics;
    Sdl.quit ();
    exit 0

let () = main ()
