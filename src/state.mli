type t

type event = 
  | Rotate 
  | Drop

type value =
  | Empty
  | Falling of int * int * int
  | Static of int * int * int

val score : t -> int

val level : t -> int

val width : t -> int

val height : t -> int

val value : t -> int -> int -> value

(* val queue : t -> tetromino? list *)

(* val held : t -> tetromino? *)

val step : t -> float -> t

val rotate : t -> [`CCW | `CW] -> t

val move : t -> [`LEFT | `RIGHT] -> t

val hold : t -> t

val hard_drop : t -> t

val handle_events : t -> (event -> unit) -> t
