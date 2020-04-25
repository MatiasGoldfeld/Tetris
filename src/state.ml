type event = 
  | Rotate 
  | Drop
  | Locking

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
  falling : Tetromino.t;
  falling_rot : int;
  (* The (x, y) position of the falling tetromino. *)
  falling_pos : int*int;
  (* The array of rows, with 0 representing the top row. The columns are arrays
     of color options, with 0 representing the left column. *)
  playfield : color option array array
}

(* matias *)
let init (level:int) : t =
  failwith "unimplemented"

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

(* angelina *)
let value (state:t) (x:int) (y:int) : v =
  failwith "unimplemented"

let queue (state:t) : Tetromino.t list =
  state.queue

let held (state:t) : Tetromino.t option =
  state.held

(* Matias *)
let step (state:t) (delta:float) (soft_drop:bool) : t =
  failwith("unimplemented")

(* Oliver *)
let rotate (state:t) (rotation:[`CCW | `CW]) : t =
  failwith("unimplemented")

(* Oliver *)
let move (state:t) (direction:[`LEFT | `RIGHT]) : t =
  failwith("unimplemented")

(* Oliver *)
let hold (state:t) : t =
  failwith "unimplemented"

(* Oliver *)
let hard_drop (state:t) : t =
  failwith "unimplemented"

let handle_events (state:t) (f:event -> unit) : t =
  List.iter f (List.rev state.events);
  {state with events=[]}
