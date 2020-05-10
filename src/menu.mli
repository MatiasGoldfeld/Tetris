(** A representation of the menu *)
type t

type mode = SinglePlayer | MultiplayerHost | MultiplayerFriend

(** [init audio graphics button_labels] initializes a menu with buttons that
    have labels [button_labels] and displays it using [graphics] with audio
    coming from [audio]. *)
val init : Audio.t -> Graphics.t -> 
  (string * string * (Menu_state.t->Menu_state.t)) list -> mode

