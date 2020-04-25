(** The representation of an a event that can be handled. *)
type event = 
  | Rotate 
  | Drop
  | Locking

type color = int * int * int

(** The representation of a value from the playfield. *)
type v =
  | Empty
  | Falling of color
  | Static of color
  | Ghost of color

(** The representation of a Tetris gamestate. *)
type t

(** [init level] is the initialized state of the game starting at [level] *)
val init : int -> t

(** [score state] is the score of [state]. *)
val score : t -> int

(** [level state] is the level of [state]. *)
val level : t -> int

(** [lines state] is the lines cleared of [state]. *)
val lines : t -> int

(** [field_width state] is the width of the [state]'s playfield. *)
val field_width : t -> int

(** [field_height state] is the height of the [state]'s playfield. *)
val field_height : t -> int

(** [value state x y] is the [v] of [state's] playfield at 
    coordinates [(x, y)] *)
val value : t -> int -> int -> v

(** [queue state] is the queue of tetrominos to be used next in the game. *)
val queue : t -> Tetromino.t list

(** [held state] is [Some t] if [t] is the tetromino held in [state],
    or [Empty] if there is none.*)
val held : t -> Tetromino.t option

(** [step state delta soft_drop] is the state of [state] after [delta] amount of
    time. [soft_drop] indicates if soft drop is active. *)
val step : t -> float -> bool -> t

(** [rotate state rotation] is [state] with an attempt to rotate the falling
    piece in [rotation]. *)
val rotate : t -> [`CCW | `CW] -> t

(** [move state direction] is [state] with an attempt to move the falling
    piece in [direction]. *)
val move : t -> [`LEFT | `RIGHT] -> t

(** [hold state] is the state of the game after attempting to hold a piece. *)
val hold : t -> t

(** [hard_drop state] is [state] with the falling piece now static 
    at the base level. *)
val hard_drop : t -> t

(** [handle_events state f] is the state after applying [f] to each event in the
    queue of events in [state] *)
val handle_events : t -> (event -> unit) -> t
