(* angelina *)
open Sdl
open Sdlmixer

exception IllegalSound of string

type t = {
  music: string;
  sound_effects : (State.event*string) list;
}

let init (path:string) = 
  Sdl.init_subsystem([`AUDIO]);
  Sdlmixer.open_audio ();
  {
    music = path^"/music/background.wav";
    sound_effects = [
      (Locking, path^"/music/selection.wav")
    ];
  }

let rec get_sound (e:State.event) = function
  | (event, sound)::t when event == e -> sound
  | h::t -> get_sound e t
  | _ -> raise (IllegalSound "No sound effect found" )

let play_sound (audio:t) (e:State.event)=
  let sound = get_sound e audio.sound_effects in
  let chunk = Sdlmixer.loadWAV sound in
  Sdlmixer.play_sound chunk

let adjust_music (audio:t) (volume:float) =
  if Sdlmixer.playing_music () then
    Sdlmixer.setvolume_music volume
  else ()

let start_music (audio:t) =
  let music = Sdlmixer.load_music audio.music in
  Sdlmixer.fadein_music music 1.0

let stop_music (audio:t) =
  if Sdlmixer.playing_music () then
    Sdlmixer.pause_music ()
  else ()

let quit (audio:t)=
  Sdlmixer.close_audio();