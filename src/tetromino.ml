
(* oliver *)
let i_default = [[0;0;0;0];[1;1;1;1];[0;0;0;0];[0;0;0;0]]
let j_default = [[1;0;0];[1;1;1];[0;0;0]]
let l_default = [[0;0;1];[1;1;1];[0;0;0]]
let o_default = [[1;1];[1;1]]
let s_default = [[0;1;1];[1;1;0];[0;0;0]]
let t_default = [[0;1;0];[1;1;1];[0;0;0]]
let z_default = [[1;1;0];[0;1;1];[0;0;0]]

let l_blue = Some (0,0,0)
let d_blue = Some (0,0,0)
let orange = Some (0,0,0)
let yellow = Some (0,0,0)
let green = Some (0,0,0)
let purple = Some (0,0,0)
let red = Some (0,0,0)


type tetromino = 
    I | J | L | O | S | T | Z

type t = tetromino

let defaults = 
  [I; J; L; O; S; T; Z]

let size = function
  | O -> 2
  | I -> 4
  | J | L | S | T | Z -> 3


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
let find_coord_val rot x y piece_list size color = 
  let change = (Float.of_int (size-1)) /. 2. in
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
  if iterator new_x new_y piece_list = 1 then color else None

let color t =
  match t with 
  | O -> yellow
  | I -> l_blue
  | L -> d_blue
  | J -> orange
  | S -> green
  | T -> purple
  | Z -> red

let value t rot column row = 
  let s = size t in
  if (column >= s || column < 0 || row >= s || row < 0) 
  then failwith "Out of bounds error"
  else
    match t with
    | O -> yellow
    | I -> find_coord_val rot column row i_default (size I) l_blue
    | L -> find_coord_val rot column row l_default (size L) d_blue
    | J -> find_coord_val rot column row j_default (size J) orange
    | S -> find_coord_val rot column row s_default (size S) green
    | T -> find_coord_val rot column row t_default (size T) purple
    | Z -> find_coord_val rot column row z_default (size Z) red