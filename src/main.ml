open Tsdl

let main () =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init "./resources/" in
    Audio.adjust_music audio 0.05;
    let game_controls = [
      (Sdl.K.left, Game.Move_left);
      (Sdl.K.right, Game.Move_right);
      (Sdl.K.up, Game.Rotate_cw);
      (Sdl.K.z, Game.Rotate_ccw);
      (Sdl.K.x, Game.Rotate_cw);
      (Sdl.K.down, Game.Soft_drop);
      (Sdl.K.space, Game.Hard_drop);
      (Sdl.K.c, Game.Hold);
    ] in
    Game.init 1 [] game_controls audio graphics;
    Sdl.quit ();
    exit 0

let () = main ()
