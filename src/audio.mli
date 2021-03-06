(** The representation of all audio used in the game. *)
type t

(** [init path] is the audio files contained at [path]. *)
val init : string -> t

(** [loop_music audio ] restarts music if it's been paused *)
val loop_music: t -> unit

(** [play_sound audio event] plays the sound triggered by [event] using [audio].
*)
val play_sound : t -> State.event -> unit

(** [adjust_music audio volume] sets the music volume to [volume].
    [volume] must be from 0 to 1. *)
val adjust_music: t -> float -> unit

(** [start_music audio] plays the designated music file of [audio]. *)
val start_music : t -> unit

(** [stop_music audio] stops the designated music file of [audio]. *)
val stop_music : t -> unit
