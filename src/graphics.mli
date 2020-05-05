(** The representation of the graphics context used to display and render. *)
type t

(** [init path] initializes the graphics system with the [path] to the resources
    folder, giving a graphics context.
    Should be called once before graphics is outputed. *)
val init : string -> t

(** [render ctx state] provides a graphical representation of [state] using
    graphics context [ctx]. *)
val render : t -> State.t -> unit
