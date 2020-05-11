(** The representation of an a event that can be handled. *)
type event = 
  | Rotate 
  | Locking
  | Movement
  | LineClear
  | EndGame

(** The representation of an RGB color. *)
type color = int * int * int

(** The representation of a value from the playfield. *)
type v =
  | Empty
  | Falling of color * int
  | Static of color
  | Ghost of color * int

(** The module type that represents a state. *)
module type S = sig
  (** The representation of a Tetris gamestate. *)
  type t

  exception Gameover of t

  (** [pauseable] is whether this state is pauseable. *)
  val pauseable : bool

  (** [score state] is the score of [state]. *)
  val score : t -> int

  (** [level state] is the level of [state]. *)
  val level : t -> int

  (** [lines state] is the lines cleared of [state]. *)
  val lines : t -> int

  (** [field_width state] is the width of the [state]'s playfield. *)
  val field_width : t -> int

  (** [field_height state] is the height of the [state]'s visible playfield. *)
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
  val update : t -> int -> bool -> t Lwt.t

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

  (** [handle_events f state] is the state after applying [f] to each event in 
      the queue of events in [state] *)
  val handle_events : (event -> unit) -> t -> t
end

(** A module that represents a local state. *)
module Local : S

(** [create_state width height level] is the initialized state of the game with
    playfield of size [width] by [height] starting at [level]. *)
val create_state : int -> int -> int -> Local.t

(** [make_test_state scoret linest levelt fall_speedt step_deltat 
    ext_placement_move_countt ext_placement_deltat min_rowt eventst queuet 
    heldt held_beforet fallingt falling_rott falling_post ghost_rowt 
    playfieldt] is the state with all of the above named parameters. *)
val make_test_state : int -> int -> int -> int-> int -> int -> int -> int ->
  event list -> Tetromino.t list -> Tetromino.t option -> bool -> Tetromino.t 
  -> int -> int * int -> int -> color option array array -> Local.t