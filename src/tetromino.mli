
(* Invariant type to describe all the different tetromino pieces. *)
type tetromino = 
    I | J | L | O | S | T | Z


(** The representation of a tetromino. *)
type t

(** [defaults] is a [t list] of possible tetromino shapes. *)
val defaults : t list

(** [size t] is the bounding box of tetromino t. *)
val size : t -> int

(** [value t rot x y] is a color option of the tetromino where [rot] is the
    rotation from 0 to 3, [x] is the x coordinate, and [y] is the y coordinate. 
*)
val value : t -> int -> int -> int -> (int * int * int) option
