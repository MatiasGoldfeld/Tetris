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

module type S = sig
  (** The representation of a game. *)
  type t

  (** [init menu_controls game_controls level audio graphics menu ] begins the game
      starting at [level], using [audio] and [graphics] contexts. Uses TSDL
      [menu_controls] and [game_controls]. *)
  val init : int -> (Sdl.keycode * menu_input) list ->
    (Sdl.keycode * game_input) list -> Audio.t -> Graphics.t -> Menu.t -> unit

  (** [in_menu game] is true if the player of [game] is in the menu. *)
  val in_menu : t -> bool
end

module Make (S : State.S) : S
