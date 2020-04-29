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
  falling_pos : int*int;
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

(** Comment this up *)
let add_to_queue q =
  q @ shuffle Tetromino.defaults

(** [drop piece state] is the [state] with [piece] initialized as the falling
    piece on the top of the playfield. *)
let drop (piece:Tetromino.t) (state:t) : t = {
  state with
  falling = piece;
  falling_rot = 0;
  falling_pos = (4, 0);
  queue =
    if List.length state.queue < (List.length Tetromino.defaults) then
      add_to_queue state.queue
    else
      state.queue
}

(** [recalculate_fall_speed state] is [state] with the fall speed adjusted to
    current level. *)
let recalculate_fall_speed (state:t) : t =
  let level_f = state.level 
                |> max 1
                |> min 15
                |> Int.to_float
  in
  let fall_speed =
    1000. *. (0.8 -. ((level_f -. 1.) *. 0.007)) ** (level_f -. 1.) in
  {state with fall_speed=Float.to_int fall_speed}

let init (width:int) (height:int) (level:int) : t =
  let first, queue =
    match shuffle Tetromino.defaults @ shuffle Tetromino.defaults with
    | h::t -> h, t
    | _ -> failwith "Unexpected empty starting queue"
  in
  {
    score = 0;
    lines = 0;
    level = level;
    fall_speed = -1;
    step_delta = 0;
    events = [];
    queue = queue;
    held = None;
    held_before = false;
    falling = first;
    falling_rot = -1;
    falling_pos = -1, -1;
    playfield = Array.make_matrix height width None
  }
  |> drop first
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
    col >= Tetromino.size falling || (begin
        match Tetromino.value falling falling_rot col row with
        | Some x -> 
          let abs_col = fst falling_pos + col in
          let abs_row = snd falling_pos + row in
          abs_col >= 0 && abs_col < field_width state &&
          abs_row >= 0 && abs_row < field_height state &&
          state.playfield.(abs_row).(abs_col) = None
        | None ->
          true
      end && check_row (col + 1) row)
  in 
  let rec check_playfield col row =
    row >= Tetromino.size falling || begin
      check_row col row && 
      check_playfield col (row + 1)
    end
  in check_playfield 0 0

let rec shadow_coordinates state column row =
  if not (legal state state.falling state.falling_rot (column, row))
  then shadow_coordinates state column (row + 1)
  else (column, row - 1)

let elem (state:t) (c:int) (r:int) =
  match state.playfield.(r).(c) with
  | Some color -> Static color
  | None ->
    let tet = state.falling in
    let fall_c, fall_r = state.falling_pos in
    let fall_rot = state.falling_rot in
    if c >= fall_c && c - fall_c < Tetromino.size state.falling then
      match Tetromino.value tet fall_rot (c-fall_c) (r-fall_r) with
      | Some color -> Falling (color, 255)
      | None ->
        let shadow_c, shadow_r = shadow_coordinates state fall_c fall_r in
        match Tetromino.value tet fall_rot shadow_c shadow_r with
        | Some color -> Ghost (color, 100)
        | None -> Empty
    else
      Empty

let value (state:t) (c:int) (r:int) : v =
  if r >= 0 && c >= 0 && r < field_height state && c < field_width state then 
    (elem state c r)
  else
    raise (InvalidCoordinates ((string_of_int c) ^", " ^ (string_of_int r)))

let queue (state:t) : Tetromino.t list =
  state.queue

let held (state:t) : Tetromino.t option =
  state.held


(** [step state] is the [state] after the falling piece has stepped down. *)
let step (state:t) : t =
  let new_pos = (fst state.falling_pos, snd state.falling_pos + 1) in
  if legal state state.falling state.falling_rot new_pos then
    { state with falling_pos = new_pos; step_delta = 0; }
  else begin
    (for column = fst state.falling_pos to
        (fst state.falling_pos + 
         (Tetromino.size state.falling - 1)) do
       for row = fst state.falling_pos to
           (snd state.falling_pos + 
            (Tetromino.size state.falling - 1)) do 
         let new_val = Tetromino.value state.falling 
             state.falling_rot 
             (column-(fst state.falling_pos)) 
             (row-(snd state.falling_pos)) in
         if new_val <> None
         then state.playfield.(row).(column) <- new_val
         else ()
       done
     done);
    drop (List.hd state.queue) 
      {state with held_before = false;
                  queue = List.tl state.queue; 
                  step_delta = 0}
  end

let update (state:t) (delta:int) (soft_drop:bool) : t =
  let new_delta = state.step_delta + delta * if soft_drop then 20 else 1 in
  let state = {state with step_delta=new_delta} in
  if state.step_delta >= state.fall_speed then
    step state
  else state

let cw013 = [(0,0);(-1,0);(-1,1);(0,-2);(-1,-2)]
let ccw103 = [(0,0);(1,0);(1,-1);(0,2);(1,2)]
let cw123 = [(0,0);(1,0);(1,-1);(0,2);(1,2)]
let ccw213 = [(0,0);(-1,0);(-1,1);(0,-2);(-1,-2)]
let cw233 = [(0,0);(1,0);(1,1);(0,-2);(1,-2)]
let ccw323 = [(0,0);(-1,0);(-1,-1);(0,2);(-1,2)]
let cw303 = [(0,0);(-1,0);(-1,-1);(0,2);(-1,2)]
let ccw033 = [(0,0);(1,0);(1,1);(0,-2);(1,-2)]

let cw014 = [(0,0);(-2,0);(1,0);(-2,-1);(1,2)]
let ccw104 = [(0,0);(2,0);(-1,0);(2,1);(-1,-2)]
let cw124 = [(0,0);(-1,0);(2,0);(-1,2);(2,-1)]
let ccw214 = [(0,0);(1,0);(-2,0);(1,-2);(-2,1)]
let cw234 = [(0,0);(2,0);(-1,0);(2,1);(-1,-2)]
let ccw324 = [(0,0);(-2,0);(1,0);(-2,-1);(1,2)]
let cw304 = [(0,0);(1,0);(-2,0);(1,-2);(-2,1)]
let ccw034 = [(0,0);(-1,0);(2,0);(-1,2);(2,-1)]

let rec test_rot state attempted_rot list =
  match list with
  | [] -> state
  | h::t -> if legal state state.falling attempted_rot 
      (fst h + fst state.falling_pos, snd h + snd state.falling_pos)
    then {state with falling_rot = attempted_rot; 
                     falling_pos = fst h + fst state.falling_pos, 
                                   snd h + snd state.falling_pos}
    else test_rot state attempted_rot t

let rotate (rotation:[`CCW | `CW]) (state:t) : t =
  let size = Tetromino.size state.falling in
  let rot = state.falling_rot in
  if size = 2 
  then state
  else
    match rotation with
    | `CW when rot = 0 -> if size = 3
      then test_rot state 1 cw013
      else test_rot state 1 cw014
    | `CW when rot = 1 -> if size = 3
      then test_rot state 2 cw123
      else test_rot state 2 cw124
    | `CW when rot = 2 -> if size = 3
      then test_rot state 3 cw233
      else test_rot state 3 cw234
    | `CW when rot = 3 -> if size = 3
      then test_rot state 0 cw303
      else test_rot state 0 cw304
    | `CCW when rot = 0 -> if size = 3
      then test_rot state 3 ccw033
      else test_rot state 3 ccw034
    | `CCW when rot = 1 -> if size = 3
      then test_rot state 0 ccw103
      else test_rot state 0 ccw104
    | `CCW when rot = 2 -> if size = 3
      then test_rot state 1 ccw213
      else test_rot state 1 ccw214
    | `CCW when rot = 3 -> if size = 3
      then test_rot state 2 ccw323
      else test_rot state 2 ccw324
    | _ -> failwith "impossible rotation"

let move (direction:[`Left | `Right]) (state:t) : t =
  match direction with
  | `Left -> if legal state (state.falling) state.falling_rot 
      (fst state.falling_pos - 1,
       snd state.falling_pos)
    then {state with falling_pos = (fst state.falling_pos - 1,
                                    snd state.falling_pos)}
    else state
  | `Right -> if legal state (state.falling) state.falling_rot 
      (fst state.falling_pos + 1,
       snd state.falling_pos)
    then {state with falling_pos = (fst state.falling_pos + 1,
                                    snd state.falling_pos)}
    else state

let hold (state:t) : t =
  match held state with
  | None -> drop (List.hd state.queue) 
              {state with held = Some state.falling;
                          held_before = true; queue = List.tl state.queue}
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
  {state with events=[]}
