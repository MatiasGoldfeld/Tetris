type event = 
  | Rotate 
  | Drop
  | Locking

type color = int * int * int

exception InvalidCoordinates 

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
  state (* failwith "Unimplemented" *)

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

<<<<<<< HEAD
(** [check_rows state falling falling_rot falling_pos column row size] 
    is true if the anticipated movement [falling] [falling_rot] [falling_pos] is 
    allowed for a specific row. False otherwise.*)
let rec check_rows state falling falling_rot falling_pos column row size =
  match Tetromino.value falling falling_rot column row with
  | Some x when column < size -> if (column < 0 || column >= field_width state||
                                     row <= field_height state ||
                                     (state.playfield.(fst falling_pos + column).(snd falling_pos + row)) <> None) 
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


(** [is_conflict state falling falling_rot falling_pos] is true if
    the anticipated movement [falling] [falling_rot] [falling_pos] is allowed.
    False otherwise. *)
let is_conflict state falling falling_rot falling_pos =
  let size = Tetromino.size state.falling in
  check_columns state falling falling_rot falling_pos 0 0 size


let empty_or_ghost (state:t) (r:int) (c:int) =
  let pos = state.falling_pos in
  let tet = state.falling in
  let rot = state.falling_rot in
  if is_conflict state tet rot pos then Empty
  else match (Tetromino.color tet) with
    | Some color -> Ghost color
    | None -> Empty

let elem (state:t) (r:int) (c:int) =
  match state.playfield.(r).(c) with
  | None -> empty_or_ghost state r c
  | Some color when 
      r < (field_height state) - state.level - Tetromino.size state.falling
    -> Falling color
  | Some color -> Static color

(* angelina *)
let value (state:t) (r:int) (c:int) : v =
  if r >= 0 && c >= 0 && r < field_height state && c < field_width state then 
    (elem state r c)
  else
    raise InvalidCoordinates
=======
let value (state:t) (x:int) (y:int) : v =
  if Random.bool () then Static (255, 100, 100) else Empty
(* failwith "unimplemented" *)
>>>>>>> 07edeae0bd71e35d142c4bc5a408bc94ce847b87

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

(* * Comment this up
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
   | _ -> true *)

(* Oliver *)
let move (state:t) (direction:[`LEFT | `RIGHT]) : t =
  match direction with
  | `LEFT -> if is_conflict state (state.falling) state.falling_rot 
      (fst state.falling_pos - 1,
       snd state.falling_pos)
    then {state with falling_pos = (fst state.falling_pos - 1,
                                    snd state.falling_pos)}
    else state
  | `RIGHT -> if is_conflict state (state.falling) state.falling_rot 
      (fst state.falling_pos + 1,
       snd state.falling_pos)
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
