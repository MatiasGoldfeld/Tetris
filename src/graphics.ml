let render (state:State.t) : unit =
  failwith "unimplemented"

(** [draw_playfield state size] is the rendered playfield of [state] with a 
    tile length of [size]. *)
let draw_playfield (state:State.t) (size:int) : Sdlvideo.surface =
  let w = State.field_width state * size in
  let h = State.field_height state * size in 
  Sdlvideo.create_RGB_surface_format (Sdlvideo.get_video_surface ()) [] w h
