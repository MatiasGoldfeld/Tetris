open Tsdl

type menu_input = 
  | MMenu
  | MLeft
  | MRight
  | MUp
  | MDown
  | MEnter

type game_input = 
  | GMenu
  | GLeft
  | GRight
  | GCW
  | GCCW
  | GSoft
  | GHard
  | GHold

module type S = sig
  type t
  val init : int -> (Sdl.keycode * menu_input) list ->
    (Sdl.keycode * game_input) list -> Audio.t -> Graphics.t -> Menu.t -> unit
  val in_menu : t -> bool
end

module Make (S : State.S) = struct
  module GR = Graphics.MakeGameRenderer (S)

  type menu_inputs_t = (Sdl.keycode, (t -> t) * bool) Hashtbl.t
  and game_inputs_t = {
    event_driven : (Sdl.keycode, (t -> t) * bool) Hashtbl.t;
    mutable soft_drop : Sdl.keycode;
  }

  and t = {
    state : S.t;
    last_update : int;
    menu_inputs : menu_inputs_t;
    game_inputs : game_inputs_t;
    audio : Audio.t;
    graphics : Graphics.t;
    menu: Menu.t;
    in_menu : bool;
  }

  let in_menu (game:t) : bool = game.in_menu

  (** [menu_inputs_press inputs (k, i)] maps key [k] to
      input [i] in [inputs]. *)
  let menu_inputs_press
      (inputs:menu_inputs_t)
      (k, i:Sdl.keycode * menu_input) : unit =
    let add = Hashtbl.add inputs in
    match i with
    | MMenu  -> add k ((fun x -> {x with in_menu = false}), false)
    | MLeft  -> ()
    | MRight -> ()
    | MUp    -> ()
    | MDown  -> ()
    | MEnter -> ()

  (** [game_inputs_press inputs (k, i)] maps key [k] to
      input [i] in [inputs]. *)
  let game_inputs_press
      (inputs:game_inputs_t)
      (k, i:Sdl.keycode * game_input) : unit =
    let add = Hashtbl.add inputs.event_driven in
    let state_fun f game = {game with state = f game.state} in
    match i with
    | GMenu  -> add k ((fun x -> {x with in_menu = true}), false)
    | GLeft  -> add k (state_fun (S.move `Left), true)
    | GRight -> add k (state_fun (S.move `Right), true)
    | GCW    -> add k (state_fun (S.rotate `CW), false)
    | GCCW   -> add k (state_fun (S.rotate `CCW), false)
    | GSoft  -> inputs.soft_drop <- k
    | GHard  -> add k (state_fun (S.hard_drop), false)
    | GHold  -> add k (state_fun (S.hold), false)

  let rec handle_events (game:t) : t =
    let event = Sdl.Event.create () in
    if not (Sdl.poll_event (Some event)) then game
    else handle_events begin
        match Sdl.Event.(enum (get event typ)) with
        | `Key_down ->
          let key = Sdl.Event.(get event keyboard_keycode) in
          let repeat = Sdl.Event.(get event keyboard_repeat) <> 0 in
          let table = if game.in_menu
            then game.menu_inputs
            else game.game_inputs.event_driven
          in if Hashtbl.mem table key then
            let action, repeatable = Hashtbl.find table key in
            if repeatable || not repeat then
              action game
            else game
          else game
        | `Quit ->
          Sdl.log "Quit event handled";
          Sdl.quit ();
          exit 0
        | _ -> game
      end

  let rec loop (game:t) : unit =
    Audio.loop_music game.audio;
    let game = handle_events game in
    if game.in_menu then begin
      Graphics.render_menu game.graphics game.menu;
      loop game;
    end 
    else
      let game = handle_events game in
      let time = Int32.to_int (Sdl.get_ticks ()) in
      let delta = (time - game.last_update) in
      let game =
        if delta >= 1000 / 60 then
          let state = if game.in_menu then game.state
            else
              let soft_sc = Sdl.get_scancode_from_key
                  game.game_inputs.soft_drop in
              let soft = (Sdl.get_keyboard_state ()).{soft_sc} = 1 in
              S.update game.state delta soft in
          GR.render game.graphics [game.state];
          { game with state = state; last_update = time }
        else
          game
      in loop game

  let init (level:int)
      (menu_controls:(Sdl.keycode * menu_input) list)
      (game_controls:(Sdl.keycode * game_input) list)
      (audio:Audio.t) (graphics:Graphics.t) (menu:Menu.t)=
    Random.self_init ();
    Audio.start_music audio;
    let menu_inputs = Hashtbl.create 6 in
    let game_inputs = {
      event_driven = Hashtbl.create 10;
      soft_drop = Sdl.K.unknown;
    } in
    List.iter (menu_inputs_press menu_inputs) menu_controls;
    List.iter (game_inputs_press game_inputs) game_controls;
    loop {
      state = (S.init 10 20 level);
      last_update = Int32.to_int (Sdl.get_ticks ());
      menu_inputs = menu_inputs;
      game_inputs = game_inputs;
      audio = audio;
      graphics = graphics;
      menu = menu;
      in_menu = false;
    }
end
