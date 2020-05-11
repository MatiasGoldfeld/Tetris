open Tsdl
open Lwt.Infix
open Ppx_lwt

module Local = Game.Make (State.Local)

let menu_controls = [
  (Sdl.K.escape, Game.MMenu);
  (Sdl.K.left,   Game.MLeft);
  (Sdl.K.right,  Game.MRight);
  (Sdl.K.up,     Game.MUp);
  (Sdl.K.down,   Game.MDown);
  (Sdl.K.return, Game.MEnter);
]

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
]

(** [main] loads SDL components and starts up the main menu. *)
let main : unit Lwt.t =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init false "./resources/"  in
    Audio.adjust_music audio 0.05;
    let%lwt () = State.create_state 10 20 1
                 |> Local.init audio graphics menu_controls game_controls in
    Sdl.quit ();
    exit 0

let () = Lwt_main.run main
