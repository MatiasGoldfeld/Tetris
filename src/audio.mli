(** The representation of all audio used in the game. *)
type t

(** [init path] is the audio files contained at [path]. *)
val init : string -> t

(** [play_sound audio e] plays the sound triggered by [e] using [audio]. *)
val play_sound : t -> State.event -> unit

(* val adjust_music : t ->  *)

(** [start_music audio] plays the designated music file of [audio]. *)
val start_music : t -> unit

(** [stop_music audio] stops the designated music file of [audio]. *)
val stop_music : t -> unit
