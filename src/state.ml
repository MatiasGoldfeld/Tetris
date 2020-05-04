type event = 
  | Rotate 
  | Drop
  | Locking

type color = int * int * int

exception InvalidCoordinates of string

type v =
  | Empty
  | Falling of color * int
  | Static of color
  | Ghost of color * int

type t = {
  score : int;
  lines : int;
  level : int;
  fall_speed : int;
  step_delta : int;
  events : event list;
  queue : Tetromino.t list;
  held : Tetromino.t option;
  held_before : bool;
  falling : Tetromino.t;
  falling_rot : int;
  (* The (c, r) position of the falling tetromino. *)
  falling_pos : int * int;
  (* The array of rows, with 0 representing the top row. The columns are arrays
     of color options, with 0 representing the left column. Only blocks already
     placed on the playfield are represented here. *)
  playfield : color option array array
}

(** [shuffle lst] is [lst] with its elements randomly shuffled. *)
let shuffle (lst:'a list) : 'a list =
  lst
  |> List.map (fun x -> (Random.bits (), x))
  |> List.sort compare
  |> List.map snd

(** [next_piece state] returns the next piece of the queue in [state], along
    with an updated state. *)
let next_piece (state:t) : t * Tetromino.t =
  let queue =
    if List.length state.queue - 1 < List.length Tetromino.defaults then
      state.queue @ shuffle Tetromino.defaults
    else
      state.queue
  in {state with queue = List.tl queue}, List.hd queue

(** [drop piece state] is the [state] with [piece] initialized as the falling
    piece on the top of the playfield. *)
let drop (piece:Tetromino.t) (state:t) : t = {
  state with
  falling = piece;
  falling_rot = 0;
  falling_pos = (5 - (Tetromino.size piece / 2), -1);
}

(** [recalculate_fall_speed state] is [state] with the fall speed adjusted to
    current level. *)
let recalculate_fall_speed (state:t) : t =
  let level_f = state.level - 1
                |> max 0
                |> min 14
                |> Int.to_float
  in
  let fall_speed = 1000. *. (0.8 -. (level_f *. 0.007)) ** level_f in
  { state with fall_speed = Float.to_int fall_speed }

let init (width:int) (height:int) (level:int) : t =
  let queue = shuffle Tetromino.defaults in
  {
    score = 0;
    lines = 0;
    level = level;
    fall_speed = -1;
    step_delta = 0;
    events = [];
    queue = List.tl queue;
    held = None;
    held_before = false;
    falling = List.hd queue;
    falling_rot = -1;
    falling_pos = -1, -1;
    playfield = Array.make_matrix height width None
  }
  |> drop (List.hd queue)
  |> recalculate_fall_speed

let score (state:t) : int =
  state.score

let level (state:t) : int =
  state.level

let lines (state:t) : int =
  state.lines

let field_width (state:t) : int =
  Array.length state.playfield.(0)

let field_height (state:t) : int =
  Array.length state.playfield

(** [legal state falling falling_rot falling_pos] is true if the anticipated
    movement [falling] [falling_rot] [falling_pos] is legal. False otherwise. *)
let legal state falling falling_rot falling_pos =
  let rec check_row col row =
    col >= Tetromino.size falling || begin
      match Tetromino.value falling falling_rot col row with
      | None -> true
      | Some x -> 
        let abs_col = fst falling_pos + col in
        let abs_row = snd falling_pos + row in
        abs_col >= 0 && abs_col < field_width state &&
        abs_row >= 0 && abs_row < field_height state &&
        state.playfield.(abs_row).(abs_col) = None
    end && check_row (col + 1) row
  in
  let rec check_playfield col row =
    row >= Tetromino.size falling || check_row col row &&
                                     check_playfield col (row + 1)
  in check_playfield 0 0

(** TODO: Document/fix this boi *)
let rec shadow_coordinates state column row =
  if not (legal state state.falling state.falling_rot (column, row))
  then shadow_coordinates state column (row + 1)
  else (column, row - 1)

let value (state:t) (c:int) (r:int) : v =
  if r < 0 || c < 0 || r >= field_height state || c >= field_width state
  then raise (InvalidCoordinates ((string_of_int c) ^", " ^ (string_of_int r)))
  else match state.playfield.(r).(c) with
    | Some color -> Static color
    | None ->
      let tet = state.falling in
      let fall_c, fall_r = state.falling_pos in
      let fall_rot = state.falling_rot in
      if c >= fall_c && c - fall_c < Tetromino.size state.falling then
        match Tetromino.value tet fall_rot (c - fall_c) (r - fall_r) with
        | Some color -> Falling (color, 255)
        | None ->
          let shadow_c, shadow_r = shadow_coordinates state fall_c fall_r in
          match Tetromino.value tet fall_rot shadow_c shadow_r with
          | Some color -> Ghost (color, 100)
          | None -> Empty
      else
        Empty

let queue (state:t) : Tetromino.t list =
  state.queue

let held (state:t) : Tetromino.t option =
  state.held

(** [step state] is the [state] after the falling piece has stepped down. *)
let step (state:t) : t =
  let pos_x, pos_y = state.falling_pos in
  if legal state state.falling state.falling_rot (pos_x, pos_y + 1) then
    { state with falling_pos = (pos_x, pos_y + 1); step_delta = 0; }
  else begin
    for col = pos_x to pos_x + (Tetromino.size state.falling - 1) do
      for row = pos_y to pos_y + (Tetromino.size state.falling - 1) do 
        let new_val = Tetromino.value state.falling state.falling_rot 
            (col - pos_x) (row - pos_y) in
        if new_val <> None
        then state.playfield.(row).(col) <- new_val
        else ()
      done
    done;
    let state, next = next_piece state in
    drop next { state with held_before = false; step_delta = 0; }
  end

let update (state:t) (delta:int) (soft_drop:bool) : t =
  let new_delta = state.step_delta + delta * if soft_drop then 20 else 1 in
  let state = {state with step_delta=new_delta} in
  if state.step_delta >= state.fall_speed then
    step state
  else state

let rotate (rotation:[`CCW | `CW]) (state:t) : t =
  let rot = state.falling_rot in
  let new_rot = (rot + match rotation with `CCW -> 3 | `CW -> 1) mod 4 in
  let rec test_rot = function
    | [] -> state
    | (x, y)::t ->
      let check_pos = (x + fst state.falling_pos, y + snd state.falling_pos) in
      if legal state state.falling new_rot check_pos then
        { state with falling_rot = new_rot; 
                     falling_pos = check_pos; }
      else test_rot t
  in test_rot (Tetromino.wall_kicks state.falling rot rotation)

let move (direction:[`Left | `Right]) (state:t) : t =
  let new_pos = match direction with
    | `Left -> fst state.falling_pos - 1, snd state.falling_pos
    | `Right -> fst state.falling_pos + 1, snd state.falling_pos
  in if legal state state.falling state.falling_rot new_pos then 
    { state with falling_pos = new_pos }
  else state

let hold (state:t) : t =
  match held state with
  | None ->
    let state, next = next_piece state in
    drop next { state with held = Some state.falling; held_before = true; }
  | Some p ->
    if state.held_before 
    then state
    else drop p {state with held = Some state.falling; held_before = true}

let hard_drop (state:t) : t =
  match shadow_coordinates state (fst state.falling_pos) (snd state.falling_pos) 
  with
  | (column, row) -> (for column = fst state.falling_pos to
                         (fst state.falling_pos + 
                          (Tetromino.size state.falling - 1)) do
                        for row = fst state.falling_pos to
                            (snd state.falling_pos + 
                             (Tetromino.size state.falling - 1)) do
                          let new_val = Tetromino.value state.falling 
                              state.falling_rot column row in
                          if new_val <> None
                          then state.playfield.(row).(column) <- new_val
                          else ()
                        done
                      done);
    drop (List.hd state.queue) 
      {state with held_before = true; 
                  queue = List.tl state.queue}

let handle_events (f:event -> unit) (state:t) : t =
  List.iter f (List.rev state.events);
  { state with events = [] }
