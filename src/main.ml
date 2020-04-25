(** Temporary main for testing purposes *)

let main () =
  Sdl.init [`VIDEO];
  let s = Sdlvideo.set_video_mode 400 400 [`RESIZABLE] in
  Sdltimer.delay 2000;
  let c = Sdlvideo.map_RGB s (355, 128, 128) in
  Sdlvideo.fill_rect ~rect:(Sdlvideo.rect 50 50 600 150) s c;
  Sdlvideo.flip s;
  Sdltimer.delay 20000;
  Sdl.quit ()

let () = main ()
