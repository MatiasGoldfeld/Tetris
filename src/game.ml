open Tsdl

type menu_input = 
  | Menu
  | Left
  | Right
  | Up
  | Down
  | Enter

type game_input = 
  | Menu
  | Move_left
  | Move_right
  | Rotate_cw
  | Rotate_ccw
  | Soft_drop
  | Hard_drop
  | Hold

type menu_inputs_t = unit
type game_inputs_t = {
  event_driven : (Sdl.keycode, (State.t -> State.t) * bool) Hashtbl.t;
  mutable soft_drop : Sdl.keycode;
}

type t = {
  state : State.t;
  last_update : int;
  menu_inputs : menu_inputs_t;
  game_inputs : game_inputs_t;
  audio : Audio.t;
  graphics : Graphics.t;
}

(** [menu_inputs_press inputs (k, i)] maps key [k] to input [i] in [inputs]. *)
let menu_inputs_press
    (inputs:menu_inputs_t)
    (k, i:Sdl.keycode * menu_input) : unit =
  match i with
  | Menu -> ()
  | Left -> ()
  | Right -> ()
  | Up -> ()
  | Down -> ()
  | Enter -> ()

(** [game_inputs_press inputs (k, i)] maps key [k] to input [i] in [inputs]. *)
let game_inputs_press
    (inputs:game_inputs_t)
    (k, i:Sdl.keycode * game_input) : unit =
  let add = Hashtbl.add inputs.event_driven in
  match i with
  | Menu -> ()
  | Move_left -> add k (State.move `Left, true)
  | Move_right -> add k (State.move `Right, true)
  | Rotate_cw -> add k (State.rotate `CCW, false)
  | Rotate_ccw -> add k (State.rotate `CW, false)
  | Soft_drop -> inputs.soft_drop <- k
  | Hard_drop -> add k (State.hard_drop, false)
  | Hold -> add k (State.hold, false)

let rec handle_events (game:t) : t =
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) then game
  else handle_events begin
      match Sdl.Event.(enum (get event typ)) with
      | `Key_down ->
        let key = Sdl.Event.(get event keyboard_keycode) in
        let repeat = Sdl.Event.(get event keyboard_repeat) <> 0 in
        let table = game.game_inputs.event_driven in
        if Hashtbl.mem table key then
          let action, repeatable = Hashtbl.find table key in
          if repeatable || not repeat then
            {game with state = action game.state}
          else game
        else game
      | `Quit ->
        Sdl.log "Quit event handled";
        Sdl.quit ();
        exit 0
      | _ -> game
    end

let rec loop (game:t) : unit =
  let game = handle_events game in
  let time = Int32.to_int (Sdl.get_ticks ()) in
  let delta = (time - game.last_update) in
  let game =
    if delta >= 1000 / 60 then
      let soft_drop_sc = Sdl.get_scancode_from_key game.game_inputs.soft_drop in
      let soft_drop = (Sdl.get_keyboard_state ()).{soft_drop_sc} = 1 in
      let state = State.update game.state delta soft_drop in
      Graphics.render game.graphics state;
      {game with state=state; last_update=time}
    else
      game
  in loop game

let init (level:int)
    (menu_controls:(Sdl.keycode * menu_input) list)
    (game_controls:(Sdl.keycode * game_input) list)
    (audio:Audio.t) (graphics:Graphics.t)=
  Random.self_init ();
  Audio.start_music audio;
  let menu_inputs = () in
  let game_inputs = {
    event_driven = Hashtbl.create 10;
    soft_drop = Sdl.K.unknown;
  } in
  List.iter (menu_inputs_press menu_inputs) menu_controls;
  List.iter (game_inputs_press game_inputs) game_controls;
  loop {
    state = (State.init 10 20 level);
    last_update = Int32.to_int (Sdl.get_ticks ());
    menu_inputs = menu_inputs;
    game_inputs = game_inputs;
    audio = audio;
    graphics = graphics;
  }
