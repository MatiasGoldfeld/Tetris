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
