(** A representation of the menu *)
type t

(** [init audio graphics button_labels] initializes a menu with buttons that
    have labels [button_labels] and displays it using [graphics] with audio
    coming from [audio]. *)
val init : Audio.t -> Graphics.t -> (string * string) list -> unit
