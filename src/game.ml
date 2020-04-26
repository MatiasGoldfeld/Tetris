module KeyMap = Map.Make(struct type t = Sdlkey.t let compare = compare end)

exception Quit

type t = {
  state : State.t;
  last_update : int;
  controls : (State.t -> State.t) KeyMap.t;
  audio: (Audio.t)
}

let handle_events (init_game:t) : t =
  let rec handle (game:t) =
    match Sdlevent.poll () with
    | None -> game
    | Some event ->
      match event with
      | Sdlevent.KEYDOWN key_event -> handle begin
          if KeyMap.mem key_event.keysym game.controls then
            {game with state =
                         KeyMap.find key_event.keysym game.controls game.state}
          else
            game
        end
      | Sdlevent.QUIT -> raise Quit
      | _ -> handle game
  in handle init_game


let rec loop (game:t) : unit =
  let game = handle_events game in
  let time = Sdltimer.get_ticks () in
  let delta = (time - game.last_update) in
  let game =
    if delta >= 1000 / 60 then
      let state = State.update game.state delta false in
      Graphics.render state;
      {game with state=state; last_update=time}
    else
      game
  in loop game

let init (level:int) (audio:Audio.t) =
  Random.self_init ();
  Audio.start_music audio;
  loop {
    state = (State.init 10 20 level);
    last_update = (Sdltimer.get_ticks ());
    controls = KeyMap.empty
               |> KeyMap.add Sdlkey.KEY_LEFT (State.move `LEFT)
               |> KeyMap.add Sdlkey.KEY_RIGHT (State.move `RIGHT)
               (* TODO: We need to fix CW CCW mixup *)
               |> KeyMap.add Sdlkey.KEY_UP (State.rotate `CCW)
               |> KeyMap.add Sdlkey.KEY_z (State.rotate `CW)
               |> KeyMap.add Sdlkey.KEY_x (State.rotate `CCW)
               |> KeyMap.add Sdlkey.KEY_c (State.hold)
               |> KeyMap.add Sdlkey.KEY_SPACE (State.hard_drop);
    audio = audio
  }
