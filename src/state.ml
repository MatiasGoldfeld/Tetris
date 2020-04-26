type event = 
  | Rotate 
  | Drop
  | Locking

type color = int * int * int

exception InvalidCoordinates of string

type v =
  | Empty
  | Falling of color
  | Static of color
  | Ghost of color

type t = {
  score : int;
  lines : int;
  level : int;
  since_last_step : float;
  events : event list;
  queue : Tetromino.t list;
  held : Tetromino.t option;
  held_before : bool;
  falling : Tetromino.t;
  falling_rot : int;
  (* The (c, r) position of the falling tetromino. *)
  falling_pos : int*int;
  (* The array of rows, with 0 representing the top row. The columns are arrays
     of color options, with 0 representing the left column. *)
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
let drop (piece:Tetromino.t) (state:t) : t =
  let new_queue = if List.length state.queue < 7 
    then add_to_queue state.queue
    else state.queue in
  {state with falling = piece; falling_rot = 0; falling_pos = (4, -1); 
              queue = new_queue}

let init (width:int) (height:int) (level:int) : t =
  let first, queue =
    match shuffle Tetromino.defaults @ shuffle Tetromino.defaults with
    | h::t -> h, t
    | _ -> failwith "Unexpected empty starting queue"
  in drop first {
    score = 0;
    lines = 0;
    level = level;
    since_last_step = 0.0;
    events = [];
    queue = queue;
    held = None;
    held_before = false;
    falling = first;
    falling_rot = 0;
    falling_pos = 0, 0;
    playfield = Array.make_matrix height width None
  }




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

(** [check_rows state falling falling_rot falling_pos column row size] 
    is true if the anticipated movement [falling] [falling_rot] [falling_pos] is 
    allowed for a specific row. False otherwise.*)
let rec check_rows state falling falling_rot falling_pos column row size =
  match Tetromino.value falling falling_rot column row with
  | Some x when column < size -> 
    if (column < 0 || column >= field_width state||
        row <= field_height state ||
        (state.playfield.(fst falling_pos + column).(snd falling_pos + row)) 
        <> None) 
    then false
    else check_rows state falling falling_rot falling_pos (column + 1) row size
  | None when column < size -> check_rows state falling falling_rot falling_pos 
                                 (column + 1) row size
  | _ -> true

(** [check_columns state falling falling_rot falling_pos column row size] 
    is true if the anticipated movement [falling] [falling_rot] [falling_pos] is 
    allowed. False otherwise.*)
let rec check_columns state falling falling_rot falling_pos column row size =
  if row < size 
  then ((check_rows state falling falling_rot falling_pos column 
           (row) size) && 
        (check_columns state falling falling_rot falling_pos column 
           (row+1) size))
  else true


(** [is_not_conflict state falling falling_rot falling_pos] is true if
    the anticipated movement [falling] [falling_rot] [falling_pos] is allowed.
    False otherwise. *)
let is_not_conflict state falling falling_rot falling_pos =
  let size = Tetromino.size state.falling in
  check_columns state falling falling_rot falling_pos 0 0 size

let rec shadow_coordinates state column row =
  if is_not_conflict state state.falling state.falling_rot (column, row) 
  then shadow_coordinates state column (row + 1)
  else Some (column, row-1)


let shadow_or_ghost (state:t) (c:int) (r:int) =
  match shadow_coordinates state c r with
  | Some (column, row) when row = r -> begin
      match (Tetromino.color state.falling) with
      | Some color -> Ghost color
      | _ -> Empty
    end
  | _ -> Empty

let elem (state:t) (c:int) (r:int) =
  match state.playfield.(r).(c) with
  | None -> begin
      let tet = state.falling in
      let (fall_c, fall_r) = state.falling_pos in
      let fall_rot = state.falling_rot in
      if c >= fall_c && c - fall_c < Tetromino.size state.falling then
        match Tetromino.value tet fall_rot (c-fall_c) (r-fall_r) with
        | Some color -> Falling color
        | None -> shadow_or_ghost state c r
      else
        Empty
    end
  | Some color -> Static color

(* angelina *)
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
(** [step state] is the [state] after the falling piece has stepped down. *)
let step (state:t) : t =
  if is_not_conflict state state.falling state.falling_rot 
      (fst state.falling_pos, snd state.falling_pos + 1)
  then {state with falling_pos = 
                     (fst state.falling_pos, snd state.falling_pos + 1);
                   since_last_step = 0.0}
  else begin (for column = fst state.falling_pos to
                 (fst state.falling_pos + 
                  (Tetromino.size state.falling - 1)) do
                for row = fst state.falling_pos to
                    (snd state.falling_pos + 
                     (Tetromino.size state.falling - 1)) do
                  let new_val = Tetromino.value state.falling 
                      state.falling_rot column row in
                  if new_val <> None
                  then state.playfield.(column).(row) <- new_val
                  else ()
                done
              done);
    drop (List.hd state.queue) 
      {state with held_before = false; queue = List.tl state.queue; 
                  since_last_step = 0.0}
  end

(* Matias *)
let update (state:t) (delta:float) (soft_drop:bool) : t =
  let adjust = if soft_drop then 0.5 else 1. in
  if state.since_last_step >= ((500. -. Float.of_int state.level -. 
                                (Float.of_int state.level -. 1.) *. 20.)) *. adjust 
  then step state
  else {state with since_last_step = state.since_last_step +. delta}


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
  | h::t -> if is_not_conflict state state.falling attempted_rot h 
    then {state with falling_rot = attempted_rot; falling_pos = h}
    else test_rot state attempted_rot t


(* Oliver *)
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



(* Oliver *)
let move (direction:[`LEFT | `RIGHT]) (state:t) : t =
  match direction with
  | `LEFT -> if is_not_conflict state (state.falling) state.falling_rot 
      (fst state.falling_pos - 1,
       snd state.falling_pos)
    then {state with falling_pos = (fst state.falling_pos - 1,
                                    snd state.falling_pos)}
    else state
  | `RIGHT -> if is_not_conflict state (state.falling) state.falling_rot 
      (fst state.falling_pos + 1,
       snd state.falling_pos)
    then {state with falling_pos = (fst state.falling_pos + 1,
                                    snd state.falling_pos)}
    else state

(* Oliver *)
let hold (state:t) : t =
  match held state with
  | None -> drop (List.hd state.queue) 
              {state with held = Some state.falling;
                          held_before = true; queue = List.tl state.queue}
  | Some p ->
    if state.held_before 
    then state
    else drop p {state with held = Some state.falling; held_before = true}

(* Oliver *)
let hard_drop (state:t) : t =
  match shadow_coordinates state (fst state.falling_pos) (snd state.falling_pos) 
  with
  | Some (column, row) -> (for column = fst state.falling_pos to
                              (fst state.falling_pos + 
                               (Tetromino.size state.falling - 1)) do
                             for row = fst state.falling_pos to
                                 (snd state.falling_pos + 
                                  (Tetromino.size state.falling - 1)) do
                               let new_val = Tetromino.value state.falling 
                                   state.falling_rot column row in
                               if new_val <> None
                               then state.playfield.(column).(row) <- new_val
                               else ()
                             done
                           done);
    drop (List.hd state.queue) 
      {state with held_before = true; 
                  queue = List.tl state.queue} 
  | None -> state


let handle_events (f:event -> unit) (state:t) : t =
  List.iter f (List.rev state.events);
  {state with events=[]}
