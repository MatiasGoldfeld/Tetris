open Tsdl
open Lwt.Infix
open Ppx_lwt

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
  | Playing
  | GameMenu
  | Gameover
  | GameoverHighscore

module type S = sig
  module S : State.S
  type t
  val init : Audio.t -> Graphics.t -> (Sdl.keycode * menu_input) list ->
    (Sdl.keycode * game_input) list -> S.t -> unit Lwt.t
end

module Make (S : State.S) = struct
  module S = S

  (** A module that makes a game render according to the S from Make. *)
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
    gmenu_pos : int;
    konami : int;
    quit : bool;
  }

  (** [menu_inputs_press inputs (k, i)] maps key [k] to
      input [i] in [inputs]. *)
  let menu_inputs_press
      (inputs:menu_inputs_t)
      (k, i:Sdl.keycode * menu_input) : unit =
    let add = Hashtbl.add inputs k in
    match i with
    | MMenu  -> add ((fun g -> { g with state = Playing }), false)
    | MLeft  -> ()
    | MRight -> ()
    | MUp    ->
      add ((fun x -> {x with gmenu_pos = max 0 (x.gmenu_pos - 1)}), true)
    | MDown  ->
      add ((fun x -> {x with gmenu_pos = min 1 (x.gmenu_pos + 1)}), true)
    | MEnter -> add ((fun g -> match g.gmenu_pos with
        | 0 -> { g with state = Playing }
        | 1 -> { g with quit = true }
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

  (** [konami_code key game] is the [game] updated with the state of the konami
      code. If the konami code is entered, [konami_code] is the game with duck
      mode toggled opposite to what it was. *)
  let konami_code (key:Sdl.keycode) (game:t) : t =
    let code = [
      Sdl.K.up; Sdl.K.up;
      Sdl.K.down; Sdl.K.down;
      Sdl.K.left; Sdl.K.right;
      Sdl.K.left; Sdl.K.right;
      Sdl.K.b; Sdl.K.a;
    ] in
    let next = 
      if key = List.nth code (game.konami)
      then game.konami + 1
      else 0
    in 
    if next >= List.length code
    then {game with konami = 0; graphics = Graphics.toggle_duck game.graphics}
    else {game with konami = next}

  (** [key_down_helper inputs key game repeat] is the game after handling
      the key down inputs. *)
  let key_down_helper inputs key game repeat=
    if Hashtbl.mem inputs key then
      let action, repeatable = Hashtbl.find inputs key in
      if repeatable || not repeat then
        action game
      else game
    else game

  (** [handle_events] handles all SDL events by using actions from [inputs] in
      [game]. *)
  let rec handle_events (inputs:inputs_t) (game:t) : t =
    let event = Sdl.Event.create () in
    if not (Sdl.poll_event (Some event)) 
    then game
    else handle_events inputs begin
        match Sdl.Event.(enum (get event typ)) with
        | `Key_down ->
          let key = Sdl.Event.(get event keyboard_keycode) in
          let game = konami_code key game in
          let repeat = Sdl.Event.(get event keyboard_repeat) <> 0 in
          key_down_helper inputs key game repeat
        | `Quit ->
          Sdl.log "Quit event handled from game";
          Sdl.quit ();
          exit 0
        | _ -> game
      end

  let handle_state_events (game:t) : t =
    let state, events = S.handle_events game.play_state in
    let game_ref = ref { game with play_state = state } in
    let handle_event = function
      | State.Endgame -> game_ref := { !game_ref with state = Gameover }
      | x -> Audio.play_sound game.audio x
    in List.iter handle_event events;
    !game_ref

  (** [update game delta time] is a game with all of the updated
      parts from a cycle for the game. *)
  let update game delta time : t Lwt.t = 
    match game.state with
    | Gameover -> 
      let menu = [("Gameover", false); ("Quit", true)] in
      GR.render game.graphics [game.play_state] menu;
      Lwt.return game
    | GameoverHighscore -> Lwt.return game
    | Playing | GameMenu ->
      let soft_sc = Sdl.get_scancode_from_key game.game_inputs.soft_drop in
      let soft_down = if game.state = GameMenu then false else
          (Sdl.get_keyboard_state ()).{soft_sc} = 1 in
      let%lwt play_state =
        if game.state = GameMenu && S.pauseable
        then Lwt.return game.play_state
        else S.update game.play_state delta soft_down
      in 
      let menu = if game.state = Playing then [] else
          [("Resume", game.gmenu_pos = 0); ("Quit", game.gmenu_pos = 1)] in
      GR.render game.graphics [game.play_state] menu;
      Lwt.return { game with play_state = play_state; last_update = time }

  (** [loop game] is unit with the side effect of running a game loop for the
      game. *)
  let rec loop (game:t) : unit Lwt.t =
    let%lwt () = Lwt_main.yield () in
    let game = handle_state_events game in
    let inputs = match game.state with 
      | Playing -> game.game_inputs.event_driven
      | GameMenu -> game.menu_inputs
      | Gameover ->
        let tbl = Hashtbl.create 1 in
        let quit = ((fun g -> { g with quit = true }), false) in
        Hashtbl.add tbl Sdl.K.return quit;
        Hashtbl.add tbl Sdl.K.escape quit;
        tbl
      | GameoverHighscore -> Hashtbl.create 0
    in
    let game = handle_events inputs game in
    let time = Int32.to_int (Sdl.get_ticks ()) in
    let delta = (time - game.last_update) in
    let%lwt game = if delta < 1000 / 60
      then Lwt.return game
      else update game delta time in
    if game.quit
    then (Audio.stop_music game.audio; Lwt.return ())
    else loop game

  let init (audio : Audio.t) (graphics : Graphics.t)
      (menu_controls : (Sdl.keycode * menu_input) list)
      (game_controls : (Sdl.keycode * game_input) list)
      (play_state : S.t) : unit Lwt.t =
    Audio.loop_music audio;
    Random.self_init ();
    Audio.start_music audio;
    let menu_inputs = Hashtbl.create 6 in
    let game_inputs = {
      event_driven = Hashtbl.create 10;
      soft_drop = Sdl.K.unknown;
    } in
    List.iter (menu_inputs_press menu_inputs) menu_controls;
    List.iter (game_inputs_press game_inputs) game_controls;
    let game = {
      state = Playing;
      play_state = play_state;
      last_update = Int32.to_int (Sdl.get_ticks ());
      menu_inputs = menu_inputs;
      game_inputs = game_inputs;
      audio = audio;
      graphics = graphics;
      gmenu_pos = 0;
      konami = 0;
      quit = false;
    } in
    loop game
end
