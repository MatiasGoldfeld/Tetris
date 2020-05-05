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

(** [init menu_controls game_controls level audio graphics] begins the game
    starting at [level], using [audio] and [graphics] contexts. Uses TSDL
    [menu_controls] and [game_controls]. *)
val init : int -> (Sdl.keycode * menu_input) list ->
  (Sdl.keycode * game_input) list -> Audio.t -> Graphics.t -> unit