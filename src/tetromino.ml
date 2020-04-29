type wall_kicks = {
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
  color : int * int * int;
  shape : int list list;
  wall_kicks : wall_kicks array;
}

let defaults = [
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

let max_size =
  List.map size defaults
  |> List.fold_left max 0

let pi = 4. *. atan 1.

let rec iterator x y piece_list : int =
  match piece_list with
  | [] -> failwith "List should really not be empty, lol. How'd you get here?"
  | k::t when y > 0 -> iterator x (y-1) t
  | k::t -> begin match k with
      | h::e when x > 0 -> iterator (x-1) y (e::t)
      | h::e -> h
      | _ -> failwith "How'd you get here?"
    end

(** [find_coord_val] is the value in a 2d list of ints at that coordinate 
    point according to a certain rotation of the 2d list. *)
let find_coord_val rot x y piece = 
  let change = (Float.of_int (size piece-1)) /. 2. in
  let new_x = ((((Float.of_int x) -. change) *. 
                (cos (pi /. 2. *. Float.of_int rot)) 
                -. ((Float.of_int y) -. change) *. 
                   (sin (pi /. 2. *. Float.of_int rot)) +. change) +. 0.5) 
              |> Float.to_int in
  let new_y = ((((Float.of_int y) -. change) *. 
                (cos (pi /. 2. *. Float.of_int rot)) 
                +. ((Float.of_int x) -. change) *. 
                   (sin (pi /. 2. *. Float.of_int rot)) +. change) +. 0.5) 
              |> Float.to_int in
  if iterator new_x new_y piece.shape = 1 then Some piece.color else None

let value t rot column row = 
  let s = size t in
  if (column >= s || column < 0 || row >= s || row < 0) then None
  else find_coord_val rot column row t
