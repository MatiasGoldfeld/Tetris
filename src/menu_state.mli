(** The representation of the buttons on the menu *)
type button

(** The representation of the different types of input in the menu. *)
type input_type = Button of string | Text of string

(** [m_field] is the [(label, text)] representation of a multiplayer 
    textfield*)
type m_field

(** A representation of the menu *)
type t

(** [text_fields menu] is the text input fields associated
    with multiplayer. *)
val text_fields : t -> (string * m_field) list

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

(** [toggle_multiplayer menu] toggles the game's menu, [menu], 
    to multiplayer mode. *)
val toggle_multiplayer : t -> t

(** [is_multiplayer menu] is true if multiplayer mode is selected
    in [menu] and false otherwise. *)
val is_multiplayer : t -> bool

(** [toggle_host menu] toggles the game's menu, [menu], for the current 
    user to be the host. *)
val toggle_host : t -> t

(** [is_host menu] is true if the player is hosting in this [menu]. *)
val is_host : t -> bool

(** [b_type button] is the string representation of [button] *)
val b_type : button -> string

(** [should_start_game menu] is true if the game menu has been given
    valid inputs and a game can be started. *)
val should_start_game : t -> bool

(** [set_start_game menu value] sets [menu]'s start_game to [value] *)
val set_start_game : t -> bool -> t

(** [adjust_music menu delta] is [menu] with the music's volume changed by
    [delta]. *)
val adjust_music : t -> float -> t

(** [volume menu] is the volume of music playing in [menu]. *)
val volume : t -> float

(** [update_text menu label] is the [menu] with text in the [label] 
    text input field updated. *)
val update_text : t -> string -> t

(** [address menu] is the ip address and port of the game. *)
val address : t -> string

(** [text menu label] is the text in the [label] text input field
    if [menu]. *)
val text : t -> string -> string 

(** [selected_text_field menu] is the label of the text field currently 
    selected in the menu. *)
val selected_text_field : t -> string

(** [toggle_leaderboard menu] is [menu] with leaderboard_mode toggled. *)
val toggle_leaderboard : t -> t

(** [leader_board mode menu] is true if the menu is set to display the
    leaderboard and false otherwise. *)
val leaderboard_mode : t -> bool
