type color = int * int * int

type wall_kicks_t = {
  cw : (int * int) list;
  ccw : (int * int) list;
}

let wall_kicks_3 = [|
  { ccw = [(0,0); (-1,0); (-1, 1); ( 0,-2); (-1,-2)]; 
    cw =  [(0,0); (-2,0); ( 1, 0); (-2,-1); ( 1, 2)]; };
  { ccw = [(0,0); ( 1,0); ( 1,-1); ( 0, 2); ( 1, 2)]; 
    cw =  [(0,0); ( 1,0); ( 1,-1); ( 0, 2); ( 1, 2)]; };
  { ccw = [(0,0); (-1,0); (-1, 1); ( 0,-2); (-1,-2)]; 
    cw =  [(0,0); ( 1,0); ( 1, 1); ( 0,-2); ( 1,-2)]; };
  { ccw = [(0,0); (-1,0); (-1,-1); ( 0, 2); (-1, 2)]; 
    cw =  [(0,0); (-1,0); (-1,-1); ( 0, 2); (-1, 2)]; };
|]

let wall_kicks_4 = [|
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
  color : color;
  shape : int list list;
  wall_kicks : wall_kicks_t array;
}

let defaults : t list = [
  { (* i tetromino *)
    color = (0, 255, 255);
    shape = [[0;0;0;0];[1;1;1;1];[0;0;0;0];[0;0;0;0]];
    wall_kicks = wall_kicks_4;
  };
  { (* j tetromino *)
    color = (255, 165, 0);
    shape = [[1;0;0];[1;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* l tetromino *)
    color = (0, 0, 255);
    shape = [[0;0;1];[1;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* o tetromino *)
    color = (255, 255, 0);
    shape = [[1;1];[1;1]];
    wall_kicks = wall_kicks_3;
  };
  { (* s tetromino *)
    color = (0, 128, 0);
    shape = [[0;1;1];[1;1;0];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* z tetromino *)
    color = (255, 0, 0);
    shape = [[1;1;0];[0;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
  { (* t tetromino *)
    color = (128, 0, 128);
    shape = [[0;1;0];[1;1;1];[0;0;0]];
    wall_kicks = wall_kicks_3;
  };
]

let size (piece:t) : int =
  List.length piece.shape

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
