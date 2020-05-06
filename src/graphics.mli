(** The representation of the graphics context used to display and render. *)
type t

(** [init duck_mode path ] initializes the graphics system, giving a graphics context.
    Should be called once before graphics is outputed. *)
val init : bool -> string -> t

(** [render ctx states] provides a graphical representation of all [states]
    using graphics context [ctx]. *)
val render : t -> State.t list -> unit
