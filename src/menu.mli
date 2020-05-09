type t

(** The representation of the buttons on the menu *)
type button

val init : string list -> t

val set_multiplayer_buttons : t -> (string*button) list -> t

val make_button : int*int -> int*int -> button

val update_button : int*int -> int*int -> button -> button

val update_buttons : t -> (string*button) list -> t

val get_button : t -> string -> button

val mouse_clicked : t -> (int*int) -> t

val button_selected : t -> string -> bool




