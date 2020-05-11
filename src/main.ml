open Tsdl
open Lwt.Infix
open Ppx_lwt

module Local = Game.Make (State.Local)
module Client = Game.Make (Remote.Client)
module Server = Game.Make (Remote.Server)

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

let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, 9000)

let test_server audio graphics =
  let state = Remote.create_server "" addr in
  Server.init audio graphics menu_controls game_controls state

let test_client audio graphics =
  let state = Remote.create_client "" addr in
  Client.init audio graphics menu_controls game_controls state

(** [main] loads SDL components and starts up the main menu. *)
let main : unit Lwt.t =
  match Sdl.init Sdl.Init.(events + timer) with
  | Error (`Msg e) -> Sdl.log "Main init error: %s" e; exit 1
  | Ok () ->
    let audio = Audio.init "./resources/audio/" in
    let graphics = Graphics.init false "./resources/"  in
    Audio.adjust_music audio 0.05;
    (* let%lwt () = test_server audio graphics in *)
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
