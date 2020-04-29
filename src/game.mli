open Tsdl

(** All inputs used in menus. *)
type menu_input = 
  | Menu
  | Left
  | Right
  | Up
  | Down
  | Enter

(** All inputs used in game. *)
type game_input = 
  | Menu
  | Move_left
  | Move_right
  | Rotate_cw
  | Rotate_ccw
  | Soft_drop
  | Hard_drop
  | Hold

(** [init menu_controls game_controls level audio graphics] begins the game
    starting at [level], using [audio] and [graphics] contexts. Uses TSDL
    [menu_controls] and [game_controls]. *)
val init : int -> (Sdl.keycode * menu_input) list ->
  (Sdl.keycode * game_input) list -> Audio.t -> Graphics.t -> unit