(** The representation of a tetromino. *)
type t
type color = (int*int*int)

(** [defaults] are the possible tetromino shapes. *)
val defaults : t list

(** [size piece] is the bounding box of [piece]. *)
val size : t -> int

(** [colors] are the possible tetromino colors. *)
val colors: color list

(** [max_size] is the maximum size of the default tetrominoes. *)
val max_size : int

(** [get_tet str] returns the tetromino with name str. If it does not recognize
    the string it returns the i piece. *)
val get_tet : string -> t

(** [value piece rot c r] is the grid value of [piece] where [rot] is the
    rotation from 0 to 3, and [(c, r)] are the coordinates. *)
val value : t -> int -> int -> int -> (int * int * int) option

(** [wall_kicks piece rot dir] are the possible wall kick offsets of [piece]
    when in [rot] and rotating [dir]. *)
val wall_kicks : t -> int -> [`CCW | `CW] -> (int * int) list
