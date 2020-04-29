open Tsdl

module KeyMap = Map.Make (struct type t = Sdl.keycode let compare = compare end)

type t = {
  state : State.t;
  last_update : int;
  controls : (State.t -> State.t) KeyMap.t;
  audio : Audio.t;
  graphics : Graphics.t;
}

let rec handle_events (game:t) : t =
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) then game
  else handle_events begin
      match Sdl.Event.(enum (get event typ)) with
      | `Key_down ->
        let key = Sdl.Event.(get event keyboard_keycode) in
        if KeyMap.mem key game.controls then
          {game with state = KeyMap.find key game.controls game.state}
        else
          game
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
      let soft_drop = (Sdl.get_keyboard_state ()).
                        {Sdl.get_scancode_from_key Sdl.K.down} = 1 in
      let state = State.update game.state delta soft_drop in
      Graphics.render game.graphics state;
      {game with state=state; last_update=time}
    else
      game
  in loop game

let init (level:int) (audio:Audio.t) (graphics:Graphics.t)=
  Random.self_init ();
  Audio.start_music audio;
  loop {
    state = (State.init 10 20 level);
    last_update = Int32.to_int (Sdl.get_ticks ());
    controls = KeyMap.empty
               |> KeyMap.add Sdl.K.left (State.move `LEFT)
               |> KeyMap.add Sdl.K.right (State.move `RIGHT)
               (* TODO: We need to fix CW CCW mixup *)
               |> KeyMap.add Sdl.K.up (State.rotate `CCW)
               |> KeyMap.add Sdl.K.z (State.rotate `CW)
               |> KeyMap.add Sdl.K.x (State.rotate `CCW)
               |> KeyMap.add Sdl.K.c (State.hold)
               |> KeyMap.add Sdl.K.space (State.hard_drop);
    audio = audio;
    graphics = graphics;
  }
