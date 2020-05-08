(** [color] is the RGB representation of the color of a tetromino. *)
type color = int * int * int

(** [wall_kicks_t] contains the data for how wall kicks work for a particular
    shape, rotation, and direction. *)
type wall_kicks_t = {
  cw : (int * int) list;
  ccw : (int * int) list;
}

(** [wall_kicks_static] is the set of empty wall kicks for shapes that do not
    rotate, like the O-tetromino.*)
let wall_kicks_static : wall_kicks_t array =
  let empty = { ccw = []; cw = []; } in
  [| empty; empty; empty; empty; |]

(** [wall_kicks_3] is the set of all wall kick data for size 3 shapes. *)
let wall_kicks_3 : wall_kicks_t array = [|
  { ccw = [(0,0); (-1,0); (-1, 1); ( 0,-2); (-1,-2)]; 
    cw =  [(0,0); (-2,0); ( 1, 0); (-2,-1); ( 1, 2)]; };
  { ccw = [(0,0); ( 1,0); ( 1,-1); ( 0, 2); ( 1, 2)]; 
    cw =  [(0,0); ( 1,0); ( 1,-1); ( 0, 2); ( 1, 2)]; };
  { ccw = [(0,0); (-1,0); (-1, 1); ( 0,-2); (-1,-2)]; 
    cw =  [(0,0); ( 1,0); ( 1, 1); ( 0,-2); ( 1,-2)]; };
  { ccw = [(0,0); (-1,0); (-1,-1); ( 0, 2); (-1, 2)]; 
    cw =  [(0,0); (-1,0); (-1,-1); ( 0, 2); (-1, 2)]; };
|]

(** [wall_kicks_4] is the set of all wall kick data for size 4 shapes. *)
let wall_kicks_4 : wall_kicks_t array = [|
  { ccw = [(0,0); (-1,0); ( 2,0); (-1, 2); ( 2,-1)]; 
    cw =  [(0,0); (-2,0); ( 1,0); (-2,-1); ( 1, 2)]; };
  { ccw = [(0,0); ( 2,0); (-1,0); ( 2, 1); (-1,-2)]; 
    cw =  [(0,0); (-1,0); ( 2,0); (-1, 2); ( 2,-1)]; };
  { ccw = [(0,0); ( 1,0); (-2,0); ( 1,-2); (-2, 1)]; 
    cw =  [(0,0); ( 2,0); (-1,0); ( 2, 1); (-1,-2)]; };
  { ccw = [(0,0); (-2,0); ( 1,0); (-2,-1); ( 1, 2)]; 
    cw =  [(0,0); ( 1,0); (-2,0); ( 1,-2); (-2, 1)]; };
|]

type t = {
  tetromino_type: string;
  color : color;
  shape : int list list;
  wall_kicks : wall_kicks_t array;
}

let defaults : t list = [
  { (* i tetromino *)
    tetromino_type = "i";
    color = (0, 255, 255);
    shape = [[0;0;0;0];[1;1;1;1];[0;0;0;0];[0;0;0;0]];
    wall_kicks = wall_kicks_4;
  };
  { (* j tetromino *)
    tetromino_type = "j";
    color = (255, 165, 0);
    shape = [[1;0;0];[1;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* l tetromino *)
    tetromino_type= "l";
    color = (0, 0, 255);
    shape = [[0;0;1];[1;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* o tetromino *)
    tetromino_type  = "o";
    color = (255, 255, 0);
    shape = [[1;1];[1;1]];
    wall_kicks = wall_kicks_static;
  };
  { (* s tetromino *)
    tetromino_type = "s";
    color = (0, 128, 0);
    shape = [[0;1;1];[1;1;0];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* z tetromino *)
    tetromino_type =  "z";
    color = (255, 0, 0);
    shape = [[1;1;0];[0;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* t tetromino *)
    tetromino_type = "t";
    color = (128, 0, 128);
    shape = [[0;1;0];[1;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
]

(** [get_tet_helper str lst] returns the tetromino with name str in list. This 
    is for testing purposes only. *)
let rec get_tet_helper str lst =
  match lst with
  | [] -> 
    { (* i tetromino *)
      tetromino_type = "i";
      color = (0, 255, 255);
      shape = [[0;0;0;0];[1;1;1;1];[0;0;0;0];[0;0;0;0]];
      wall_kicks = wall_kicks_4;
    }
  | h::t -> if str = h.tetromino_type then h else get_tet_helper str t


let get_tet str =
  get_tet_helper str defaults 


let size (piece:t) : int =
  List.length piece.shape

let colors =
  List.map (fun d -> d.color) defaults

let max_size : int =
  List.map size defaults
  |> List.fold_left max 0

(** [find_coord_val] is the value in a 2d list of ints at that coordinate 
    point according to a certain rotation of the 2d list. *)
let find_coord_val (piece:t) (rot:int) (x:int) (y:int) : color option = 
  let rot = Float.of_int rot in
  let x, y = Float.of_int x, Float.of_int y in
  let change = Float.of_int (size piece - 1) /. 2. in
  let new_x = ((x -. change) *. (cos (Float.pi /. -2. *. rot)) -.
               (y -. change) *. (sin (Float.pi /. -2. *. rot)) +. change) +. 0.5
              |> Float.to_int in
  let new_y = ((y -. change) *. (cos (Float.pi /. -2. *. rot)) +.
               (x -. change) *. (sin (Float.pi /. -2. *. rot)) +. change) +. 0.5
              |> Float.to_int in
  let row = List.nth piece.shape new_y in
  match List.nth row new_x with
  | 1 -> Some piece.color
  | _ -> None

let value (piece:t) (rot:int) (col:int) (row:int) : color option = 
  let s = size piece in
  if col >= s || col < 0 || row >= s || row < 0 then None
  else find_coord_val piece rot col row

let wall_kicks (piece:t) (rot:int) (dir:[`CCW | `CW]) : (int * int) list =
  match dir with
  | `CCW -> piece.wall_kicks.(rot).ccw
  | `CW -> piece.wall_kicks.(rot).cw
