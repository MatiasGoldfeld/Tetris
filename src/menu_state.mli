(** The representation of the buttons on the menu *)
type button

(** The representation of the different types of input in the menu. *)
type input_type = Button of string | Text of string

(** [m_field] is the [(label, text)] representation of a multiplayer 
    textfield*)
type m_field

(** A representation of the menu *)
type t

(** [multiplayer_fields menu] is the text input fields associated
    with multiplayer. *)
val multiplayer_fields : t -> (string * m_field) list

(** [ make_button coords size on_click ] creates a button at coordinates 
    [coords] and of size [size]. When the button is clicked, [on_click] is
    called. *)
val make_button : int*int -> int*int -> string -> (t->t)-> button

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

(** [init buttons] initializes a menu with buttons. *)
val init : (string * string * (t->t)) list -> t

val toggle_multiplayer : t -> t

val is_multiplayer : t -> bool

val b_type : button -> string

val should_quit_menu : t -> bool

val start_game : t  -> t
