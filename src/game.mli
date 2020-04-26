(** [Quit] is thrown when the user quits the program unexpectedly. *)
exception Quit 

(** [init level] begins the game starting at [level]. *)
val init : int -> Audio.t -> unit