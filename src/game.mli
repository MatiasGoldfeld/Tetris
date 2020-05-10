open Tsdl

type menu_input = 
  | MMenu
  | MLeft
  | MRight
  | MUp
  | MDown
  | MEnter

type game_input = 
  | GMenu
  | GLeft
  | GRight
  | GCW
  | GCCW
  | GSoft
  | GHard
  | GHold


(** The type of a game module. *)
module type S = sig

  (** The representation of a game. *)
  type t

  (** [init audio graphics level menu_controls game_controls] begins the game
      starting at [level], using [audio] and [graphics] contexts. Uses Tsdl
      [menu_controls] and [game_controls]. *)
  val init :  Audio.t -> Graphics.t -> int -> (Sdl.keycode * menu_input) list ->
    (Sdl.keycode * game_input) list -> unit
end

(** The module that makes a state adherent to S. *)
module Make (S : State.S) : S
