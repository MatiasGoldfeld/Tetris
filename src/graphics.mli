(** The representation of the graphics context used to display and render. *)
type t

(** [init duck_mode path ] initializes the graphics system, giving a graphics context.
    Should be called once before graphics is outputed. *)
val init : bool -> string -> t

(** [render_menu ctx menu] renders the menu and gives a representation of the
    buttons in the menu *)
val render_menu : t -> Menu.t -> (string*Menu.button) list

module type GameRenderer = sig
  module S : State.S
  (** [render ctx states menu] provides a graphical representation of all
      [states] and pause menu [menu] using graphics context [ctx]. *)
  val render : t -> S.t list -> (string * bool) list -> unit
end

module MakeGameRenderer (S : State.S) :
  GameRenderer with module S = S


