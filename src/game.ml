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

type game_state =
  | MainMenu
  | Playing
  | GameMenu
  | Gameover
  | GameoverHighscore

module type Button = sig
  type t
  val onPress : t -> t
  val coords : t -> (int*int)
end

module type S = sig
  type t
  val init : int -> (Sdl.keycode * menu_input) list ->
    (Sdl.keycode * game_input) list -> Audio.t -> Graphics.t -> Menu.t -> unit
  val in_menu : t -> bool
end

module Make (S : State.S) = struct
  module GR = Graphics.MakeGameRenderer (S)

  type inputs_t = (Sdl.keycode, (t -> t) * bool) Hashtbl.t
  and menu_inputs_t = inputs_t
  and game_inputs_t = {
    event_driven : inputs_t;
    mutable soft_drop : Sdl.keycode;
  }

  and t = {
    state : game_state;
    play_state : S.t;
    last_update : int;
    menu_inputs : menu_inputs_t;
    game_inputs : game_inputs_t;
    audio : Audio.t;
    graphics : Graphics.t;
    menu : Menu.t;
    gmenu_pos : int;
  }

  let in_menu (game:t) : bool = game.state = GameMenu

  (** [menu_inputs_press inputs (k, i)] maps key [k] to
      input [i] in [inputs]. *)
  let menu_inputs_press
      (inputs:menu_inputs_t)
      (k, i:Sdl.keycode * menu_input) : unit =
    let add = Hashtbl.add inputs k in
    match i with
    | MMenu  -> add ((fun x -> {x with state = Playing}), false)
    | MLeft  -> ()
    | MRight -> ()
    | MUp    ->
      add ((fun x -> {x with gmenu_pos = max 0 (x.gmenu_pos - 1)}), true)
    | MDown  ->
      add ((fun x -> {x with gmenu_pos = min 1 (x.gmenu_pos + 1)}), true)
    | MEnter -> add ((fun x -> match x.gmenu_pos with
        | 0 -> {x with state = Playing}
        | 1 -> {x with state = MainMenu}
        | _ -> failwith "Invalid in-game menu position"
      ), false)

  (** [game_inputs_press inputs (k, i)] maps key [k] to
      input [i] in [inputs]. *)
  let game_inputs_press
      (inputs:game_inputs_t)
      (k, i:Sdl.keycode * game_input) : unit =
    let add = Hashtbl.add inputs.event_driven k in
    let state_fun f game = {game with play_state = f game.play_state} in
    match i with
    | GMenu  -> add ((fun x -> {x with state = GameMenu; gmenu_pos = 0}), false)
    | GLeft  -> add (state_fun (S.move `Left), true)
    | GRight -> add (state_fun (S.move `Right), true)
    | GCW    -> add (state_fun (S.rotate `CW), false)
    | GCCW   -> add (state_fun (S.rotate `CCW), false)
    | GSoft  -> inputs.soft_drop <- k
    | GHard  -> add (state_fun (S.hard_drop), false)
    | GHold  -> add (state_fun (S.hold), false)

  let rec handle_events (inputs:inputs_t) (game:t) : t =
    let event = Sdl.Event.create () in
    if not (Sdl.poll_event (Some event)) then game
    else handle_events inputs begin
        match Sdl.Event.(enum (get event typ)) with
        | `Key_down ->
          let key = Sdl.Event.(get event keyboard_keycode) in
          let repeat = Sdl.Event.(get event keyboard_repeat) <> 0 in
          if Hashtbl.mem inputs key then
            let action, repeatable = Hashtbl.find inputs key in
            if repeatable || not repeat then
              action game
            else game
          else game
        | `Mouse_button_down ->
          if game.state <> MainMenu then game else
            let click_coords = (Sdl.Event.(get event mouse_button_x), 
                                Sdl.Event.(get event mouse_button_x)) in 
            let menu = Menu.mouse_clicked game.menu click_coords in
            { game with menu = menu }
        | `Quit ->
          Sdl.log "Quit event handled";
          Sdl.quit ();
          exit 0
        | _ -> game
      end

  let rec loop (game:t) : unit =
    Audio.loop_music game.audio;
    let inputs = match game.state with 
      | MainMenu -> Hashtbl.create 0
      | Playing -> game.game_inputs.event_driven
      | GameMenu -> game.menu_inputs
      | Gameover -> Hashtbl.create 0
      | GameoverHighscore -> Hashtbl.create 0
    in
    let game = handle_events inputs game in
    let time = Int32.to_int (Sdl.get_ticks ()) in
    let delta = (time - game.last_update) in
    let game =
      if delta < 1000 / 60 then game else
        match game.state with
        | MainMenu ->
          print_endline "main menu";
          let menu = Graphics.render_menu game.graphics game.menu in
          {game with menu = menu}
        | Gameover -> game
        | GameoverHighscore -> game
        | Playing | GameMenu ->
          let soft_sc = Sdl.get_scancode_from_key game.game_inputs.soft_drop in
          let soft_down = if game.state = GameMenu then false else
              (Sdl.get_keyboard_state ()).{soft_sc} = 1 in
          let play_state =
            if game.state = GameMenu && S.pauseable
            then game.play_state
            else S.update game.play_state delta soft_down
          in 
          let menu = if game.state = Playing then [] else
              [("Resume", game.gmenu_pos = 0); ("Quit", game.gmenu_pos = 1)] in
          GR.render game.graphics [game.play_state] menu;
          { game with play_state = play_state; last_update = time }
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
      state = MainMenu;
      play_state = S.init 10 20 level;
      last_update = Int32.to_int (Sdl.get_ticks ());
      menu_inputs = menu_inputs;
      game_inputs = game_inputs;
      audio = audio;
      graphics = graphics;
      menu = menu;
      gmenu_pos = 0;
    }
end
