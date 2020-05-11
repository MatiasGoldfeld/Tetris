(** The representation of a highscore of a user. *)
type t

(** [high_score_getter unit] is the type t list of high scores from a secret 
    highscore file. If this file does not exist, it creates it and returns an 
    empty list.*)
val high_score_getter : unit -> t list


(** [write_high_score highscore] writes [highscore] to a secret highscore file.
    If this file does not exist, it creates it and adds [highscore] to it. *)
val write_high_score : t -> unit











