type t

(** The representation of the buttons on the menu *)
type button

val init : unit -> t

val set_multiplayer_buttons : t -> (string*button) list -> t

val make_button : int*int -> int*int -> button

val mouse_clicked : t -> (int*int) -> t




