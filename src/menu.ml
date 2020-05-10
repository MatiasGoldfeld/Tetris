open Tsdl

type mode = SinglePlayer | MultiplayerHost | MultiplayerFriend

type t = {
  menu : Menu_state.t;
  audio : Audio.t;
  graphics : Graphics.t;
  last_update : int;
}

module type Button = sig
  type t
  val onPress : t -> t
  val coords : t -> (int*int)
end

module LocalGame = Game.Make (State.Local)

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

(** [handle_events] handles all SDL events by using actions from [inputs] in
    [game]. *)
let rec handle_events (menu : t) : t =
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) 
  then menu
  else handle_events begin
      match Sdl.Event.(enum (get event typ)) with
      | `Mouse_button_down ->
        let click_coords = (Sdl.Event.(get event mouse_button_x), 
                            Sdl.Event.(get event mouse_button_y)) in 
        { menu with menu = Menu_state.mouse_clicked menu.menu click_coords }
      | `Quit ->
        Sdl.log "Quit event handled from main menu";
        Sdl.quit ();
        exit 0
      | _ -> menu
    end

(** [loop menu] is unit with byproduct of running the menu loop. *)
let rec loop (menu : t) : unit =
  let menu = handle_events menu in
  let time = Int32.to_int (Sdl.get_ticks ()) in
  let delta = (time - menu.last_update) in
  let menu =
    if Menu_state.should_start_game menu.menu then begin
      if Menu_state.is_multiplayer menu.menu then
        ()
      else
        LocalGame.init menu.audio menu.graphics 0 menu_controls game_controls;
      { menu with menu = Menu_state.set_start_game menu.menu false }
    end else menu in
  let menu = if delta < 1000 / 60 then menu else
      { menu with menu = Graphics.render_menu menu.graphics menu.menu }
  in loop menu

let init audio graphics labels = 
  loop {
    menu = Menu_state.init labels;
    audio = audio;
    graphics = graphics;
    last_update = Int32.to_int @@ Sdl.get_ticks ();
  }