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


(** The module which runs, manages state, handles input, and displays a game. *)
module type S = sig
  module S : State.S

  (** The representation of a game. *)
  type t

  (** [init audio graphics menu_controls game_controls state username] 
      begins the game
      starting at [state], using [audio] and [graphics] contexts. Uses Tsdl
      [menu_controls] and [game_controls]. *)
  val init : Audio.t -> Graphics.t -> (Sdl.keycode * menu_input) list ->
    (Sdl.keycode * game_input) list -> S.t -> string -> unit Lwt.t
end

(** The module that makes a state adherent to S. *)
module Make (S : State.S) : S with module S = S
