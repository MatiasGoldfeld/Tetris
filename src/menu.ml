open Tsdl
open Lwt.Infix
open Ppx_lwt

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
      | `Key_down -> 
        let key = (Sdl.Event.(get event keyboard_keycode)) in begin
          if key = Sdl.K.backspace then
            let text = Menu_state.address menu.menu in
            let text_length = String.length text in
            if text <> "" then 
              let updated_menu = String.sub text 0 (text_length-1) 
                                 |> Menu_state.update_address menu.menu in
              {menu with menu = updated_menu }
            else menu
          else menu
        end
      | `Text_input -> 
        let text = Sdl.Event.(get event text_input_text) in
        let address = Menu_state.address menu.menu in
        let updated_menu = Menu_state.update_address (menu.menu) 
            (address^text) in
        {menu with menu = updated_menu}
      | `Quit ->
        Sdl.log "Quit event handled from main menu";
        Sdl.quit ();
        exit 0
      | _ -> menu
    end

let adjust_music menu delta =
  let audio = menu.audio in
  Audio.adjust_music audio delta

let start_multiplayer_game (menu : t) : unit Lwt.t =
  match String.split_on_char ':' (Menu_state.address menu.menu) with
  | ip :: port :: [] ->
    let username = "" in
    let addr = Lwt_unix.ADDR_INET
        (Unix.inet_addr_of_string ip, int_of_string port) in
    if Menu_state.is_host menu.menu then
      let state = Remote.create_server username addr in
      Server.init menu.audio menu.graphics menu_controls game_controls state
    else
      let state = Remote.create_client username addr in
      Client.init menu.audio menu.graphics menu_controls game_controls state
  | _ -> Lwt.return ()

(** [loop menu] runs the main menu loop, handling input and rendering the
    menu. *)
let rec loop (menu : t) : unit Lwt.t =
  let%lwt () = Lwt_main.yield () in
  let audio = menu.audio in
  Menu_state.volume menu.menu |> Audio.adjust_music audio;
  let menu = handle_events menu in
  let time = Int32.to_int (Sdl.get_ticks ()) in
  let delta = (time - menu.last_update) in
  let%lwt menu =
    if Menu_state.should_start_game menu.menu then 
      let%lwt () =
        if Menu_state.is_multiplayer menu.menu then
          start_multiplayer_game menu
        else
          State.create_state 10 20 1
          |> Local.init menu.audio menu.graphics menu_controls game_controls
      in Lwt.return
        { menu with menu = Menu_state.set_start_game menu.menu false }
    else Lwt.return menu in
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