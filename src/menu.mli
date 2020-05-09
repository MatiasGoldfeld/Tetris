type t

(** The representation of the buttons on the menu *)
type button

(** [init button_labels] initializes a menu with buttons that have labels
    [button_labels] *)
val init : string list -> t

(** [ make_button coords size ] creates a button at coordinates [coords]
    and of size [size] *)
val make_button : int*int -> int*int -> button

(** [ update_button coords size ] updates a button, giving it new coordinates
    [coords] and size [size] *)
val update_button : int*int -> int*int -> button -> button

(** [update_buttons buttons] updates the buttons in the menu *)
val update_buttons : t -> (string*button) list -> t

(** [get_button menu label] gets the button with [label] in [menu] *)
val get_button : t -> string -> button

(** [buttons menu] is the butttons in [menu] *)
val buttons: t -> (string*button) list

(** [mouse_clicked menu coords] updates and buttons in [menu] that may
    have been selected by the mouse click *)
val mouse_clicked : t -> (int*int) -> t

(** [button_selected menu label] is true if the button in [menu] with [label] \
    has been selected by the user. *)
val button_selected : t -> string -> bool




