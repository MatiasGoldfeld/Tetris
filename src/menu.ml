open Tsdl
open Tsdl_ttf

type t = {
  multiplayer: bool;
  volume: float;
  level: int;
}

let init () = {
  multiplayer= false;
  volume= 0.05;
  level= 0
}



