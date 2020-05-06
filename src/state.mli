(** The representation of an a event that can be handled. *)
type event = 
  | Rotate 
  | Drop
  | Locking

module type S = sig
  (** The representation of an RGB color. *)
  type color = int * int * int

  (** The representation of a value from the playfield. *)
  type v =
    | Empty
    | Falling of color * int
    | Static of color
    | Ghost of color * int

  (** The representation of a Tetris gamestate. *)
  type t

  (** [pauseable] is whether this state is pauseable. *)
  val pauseable : bool

  (** [init width height level] is the initialized state of the game with
      playfield of size [width] by [height] starting at [level]. *)
  val init : int -> int -> int -> t

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

  (** [value state c r] is the [v] of [state's] playfield at 
      coordinates [(c, r)] *)
  val value : t -> int -> int -> v

  (** [queue state] is the queue of tetrominos to be used next in the game. *)
  val queue : t -> Tetromino.t list

  (** [held state] is [Some t] if [t] is the tetromino held in [state],
      or [Empty] if there is none.*)
  val held : t -> Tetromino.t option

  (** [update state delta soft_drop] is the state of [state] after [delta]
      amount of time in milliseconds. [soft_drop] indicates if soft drop is
      active. *)
  val update : t -> int -> bool -> t

  (** [rotate rotation state] is [state] with an attempt to rotate the falling
      piece in [rotation]. *)
  val rotate : [`CCW | `CW] -> t -> t

  (** [move direction state] is [state] with an attempt to move the falling
      piece in [direction]. *)
  val move : [`Left | `Right] -> t -> t

  (** [hold state] is the state of the game after attempting to hold a piece. *)
  val hold : t -> t

  (** [hard_drop state] is [state] with the falling piece now static 
      at the base level. *)
  val hard_drop : t -> t

  (** [handle_events f state] is the state after applying [f] to each event in the
      queue of events in [state] *)
  val handle_events : (event -> unit) -> t -> t
end

module Local : S
