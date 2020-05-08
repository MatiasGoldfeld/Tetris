(** The representation of the graphics context used to display and render. *)
type t

(** [init duck_mode path ] initializes the graphics system, giving a graphics context.
    Should be called once before graphics is outputed. *)
val init : bool -> string -> t

val render_menu : t -> Menu.t -> unit

module type GameRenderer = sig
  module S : State.S
  (** [render ctx states] provides a graphical representation of all [states]
      using graphics context [ctx]. *)
  val render : t -> S.t list -> unit
end

module MakeGameRenderer (S : State.S) :
  GameRenderer with module S = S


