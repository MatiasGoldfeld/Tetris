(* angelina *)

exception IllegalSound of string

type t = {
  music: string;
  sound_effects : (State.event*string) list;
}

let init (path:string) = 
  Sdlmixer.open_audio ();
  {
    music = path^"/music/background.mp4";
    sound_effects = [];
  }

let rec get_sound (e:State.event) = function
  | (event, sound)::t when event == e -> sound
  | h::t -> get_sound e t
  | _ -> raise (IllegalSound "No sound effect found" )

let play_sound (audio:t) (e:State.event)=
  let sound = get_sound e audio.sound_effects in
  Sdlmixer.play_sound sound

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