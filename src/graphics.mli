(** [init] initializes the graphics system. Should be called once before
    graphics is outputed. *)
val init : unit -> unit

(** [render state] provides a graphical representation of [state]. *)
val render : State.t -> unit
