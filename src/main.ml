open Tsdl

module LocalGame = Game.Make (State.Local)

let main () =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init false "./resources/"  in
    let menu = Menu.init () in
    Audio.adjust_music audio 0.05;
    let menu_controls = [
      (Sdl.K.escape,   Game.MMenu);
      (Sdl.K.left,     Game.MLeft);
      (Sdl.K.right,    Game.MRight);
      (Sdl.K.up,       Game.MUp);
      (Sdl.K.down,     Game.MDown);
      (Sdl.K.return,   Game.MEnter);
    ] in
    let game_controls = [
      (Sdl.K.escape, Game.GMenu);
      (Sdl.K.left,   Game.GLeft);
      (Sdl.K.right,  Game.GRight);
      (Sdl.K.up,     Game.GCW);
      (Sdl.K.z,      Game.GCCW);
      (Sdl.K.x,      Game.GCW);
      (Sdl.K.down,   Game.GSoft);
      (Sdl.K.space,  Game.GHard);
      (Sdl.K.c,      Game.GHold);
    ] in
    LocalGame.init 1 menu_controls game_controls audio graphics menu;
    Sdl.quit ();
    exit 0

let () = main ()
