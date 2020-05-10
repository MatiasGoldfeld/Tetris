(** A representation of the menu *)
type t

(** The representation of the buttons on the menu *)
type button

(** The representation of the different types of input in the menu. *)
type input_type = Button of string | Text of string

(** [m_field] is the [(label, text)] representation of a multiplayer 
    textfield*)
type m_field

(** [init gfx button_labels] initializes a menu with buttons that have labels
    [button_labels] and displays it using rendering function [gfx]. *)
val init : (t -> t) -> (string*string) list -> unit

(** [multiplayer_fields menu] is the text input fields associated
    with multiplayer. *)
val multiplayer_fields : t -> (string * m_field) list

(** [ make_button coords size ] creates a button at coordinates [coords]
    and of size [size] *)
val make_button : int*int -> int*int -> string -> (unit -> unit) -> button

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




