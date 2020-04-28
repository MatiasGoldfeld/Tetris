open Tsdl
open Tsdl_mixer

type t = {
  music: Mixer.music;
  sound_effects : (State.event*Mixer.chunk) list;
}

(** [unpack message result] is [value] if [result] is [Ok value]. Otherwise, it
    logs [message] along with the error, and exits the program. *)
let unpack (message:string) (result:'a Sdl.result) : 'a =
  match result with
  | Error (`Msg e) -> Sdl.log "%s: %s" message e; Sdl.quit (); exit 1
  | Ok value -> value

let init (path:string) : t = 
  Mixer.(init Init.empty) |> unpack "Mixer init error" |> ignore;
  Mixer.(open_audio default_frequency default_format default_channels 4096)
  |> unpack "Mixer open audio error";
  let load file =
    Mixer.load_wav (path ^ file) |> unpack ("Failed loadeding " ^ file)
  in
  {
    music = Mixer.load_mus (path ^ "background.wav")
            |> unpack "Failed to load music";
    sound_effects = [
      (Locking, load "selection.wav")
    ];
  }

let play_sound (audio:t) (e:State.event) : unit =
  Mixer.play_channel (-1)  (List.assoc e audio.sound_effects) 1
  |> unpack "Failed to play sound" |> ignore

let adjust_music (audio:t) (volume:float) : unit =
  volume *. (Int.to_float Mixer.max_volume)
  |> Float.to_int
  |> max 0
  |> min Mixer.max_volume
  |> Mixer.volume_music
  |> ignore

let start_music (audio:t) : unit =
  Mixer.play_music audio.music (-1)
  |> unpack "Failed to play music"
  |> ignore

let stop_music (audio:t) : unit =
  Mixer.pause_music ()

let quit (audio:t) : unit=
  Mixer.quit ()