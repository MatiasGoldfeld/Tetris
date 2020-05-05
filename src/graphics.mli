(** The representation of the graphics context used to display and render. *)
type t

<<<<<<< HEAD
(** [init duck_mode path ] initializes the graphics system, giving a graphics context.
    Should be called once before graphics is outputed. *)
val init : bool -> string -> t
=======
(** [init path] initializes the graphics system with the [path] to the resources
    folder, giving a graphics context.
    Should be called once before graphics is outputed. *)
val init : string -> t
>>>>>>> 956a9303065f1c4eadee218d0bdd1a7c09481f05

(** [render ctx state] provides a graphical representation of [state] using
    graphics context [ctx]. *)
val render : t -> State.t -> unit
