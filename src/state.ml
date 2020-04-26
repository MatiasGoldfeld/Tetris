type event = 
  | Rotate 
  | Drop

type color = int * int * int

type v =
  | Empty
  | Falling of color
  | Static of color
  | Ghost of color

type t = {
  score : int;
  lines : int;
  level : int;
  events : event list;
  queue : Tetromino.t list;
  held : Tetromino.t option;
  held_before : bool;
  falling : Tetromino.t;
  falling_rot : int;
  (* The (x, y) position of the falling tetromino. *)
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
  failwith "Unimplemented"

let init (width:int) (height:int) (level:int) : t =
  let first, queue =
    match shuffle Tetromino.defaults @ shuffle Tetromino.defaults with
    | h::t -> h, t
    | _ -> failwith "Unexpected empty starting queue"
  in drop first {
    score = 0;
    lines = 0;
    level = level;
    events = [];
    queue = queue;
    held = None;
    held_before = false;
    falling = first;
    falling_rot = 0;
    falling_pos = 0, 0;
    playfield = Array.make_matrix height width None
  }



let rec check_rows state falling falling_rot falling_pos column row=


  let rec is_conflict state falling falling_rot falling_pos column row =
    let size = Tetromino.size state.falling in
    match Tetromino.value falling falling_rot column row with
    | Some x  when column < size ->
    | None -> 



      let shadow_coordinates = 




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

let value (state:t) (x:int) (y:int) : v =
  failwith "unimplemented"

let queue (state:t) : Tetromino.t list =
  state.queue

let held (state:t) : Tetromino.t option =
  state.held

(** [step state] is the [state] after the falling piece has stepped down. *)
let step (state:t) : t =
  failwith "unimplemented"

(* Matias *)
let update (state:t) (delta:float) (soft_drop:bool) : t =
  failwith("unimplemented")

(* Oliver *)
let rotate (state:t) (rotation:[`CCW | `CW]) : t =
  failwith("unimplemented")

(** Comment this up *)
let rec first_block_from_left falling falling_rot column row size =
  match Tetromino.value falling falling_rot column row with
  | Some x when column < size -> Some column
  | None when column < size -> 
    first_block_from_left falling falling_rot (column + 1) row size
  | _ -> None 

(** Comment this up *)
let rec first_block_from_right falling falling_rot column row size =
  match Tetromino.value falling falling_rot column row with
  | Some x when column > -1 -> Some column
  | None when column > -1 -> 
    first_block_from_right falling falling_rot (column - 1) row size
  | _ -> None 

(** Comment this up *)
let rec check_left state through =
  let size = Tetromino.size state.falling in
  match first_block_from_left state.falling state.falling_rot 0 
          through (size) with
  | Some x when through < size -> begin 
      let column = ((fst state.falling_pos) + x) in
      if (column = 0 || (state.playfield.(column-1).(through)) <> None) 
      then false 
      else check_left state (through + 1)
    end
  | None when through < size -> check_left state (through + 1)
  | _ -> true

(** Comment this up *)
let rec check_right state through =
  let size = Tetromino.size state.falling in
  match first_block_from_right state.falling state.falling_rot (size - 1) 
          through (size) with
  | Some x when through < size -> begin 
      let column = ((fst state.falling_pos) + x) in
      if (column = field_width state - 1 || 
          (state.playfield.(column+1).(through)) <> None) 
      then false 
      else check_right state (through + 1)
    end
  | None when through < size -> check_right state (through + 1)
  | _ -> true

(* Oliver *)
let move (state:t) (direction:[`LEFT | `RIGHT]) : t =
  match direction with
  | `LEFT -> if check_left state (Tetromino.size state.falling) 
    then {state with falling_pos = (fst state.falling_pos - 1,
                                    snd state.falling_pos)}
    else state
  | `RIGHT -> if check_left state (Tetromino.size state.falling) 
    then {state with falling_pos = (fst state.falling_pos + 1,
                                    snd state.falling_pos)}
    else state

(* Oliver *)
let hold (state:t) : t =
  match held state with
  | None -> 
    if List.length state.queue <= 7 
    then drop (List.hd state.queue) 
        {state with held_before = true; queue = add_to_queue 
                                            (List.tl state.queue)}
    else drop (List.hd state.queue) 
        {state with held_before = true; queue = List.tl state.queue}
  | Some p ->
    if state.held_before 
    then state
    else drop p {state with held_before = true}

(* Oliver *)
let hard_drop (state:t) : t =
  failwith "unimplemented"

let handle_events (state:t) (f:event -> unit) : t =
  List.iter f (List.rev state.events);
  {state with events=[]}
