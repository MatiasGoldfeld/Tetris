(** The representation of a tetromino. *)
type t

(** [defaults] are the possible tetromino shapes. *)
val defaults : t list

(** [size piece] is the bounding box of [piece]. *)
val size : t -> int

(** [max_size] is the maximum size of the default tetrominoes. *)
val max_size : int

(** [value piece rot c r] is the grid value of [piece] where [rot] is the
    rotation from 0 to 3, and [(c, r)] are the coordinates. *)
val value : t -> int -> int -> int -> (int * int * int) option

(** [wall_kicks piece rot dir] are the possible wall kick offsets of [piece]
    when in [rot] and rotating [dir]. *)
val wall_kicks : t -> int -> [`CCW | `CW] -> (int * int) list
