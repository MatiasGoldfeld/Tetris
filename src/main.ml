(** Temporary main for testing purposes *)

<<<<<<< HEAD
let main () = 
  Sdl.init [`VIDEO];
  let s = Sdlvideo.set_video_mode 400 400 [`RESIZABLE] in
  Sdltimer.delay 2000;
  let c = Sdlvideo.map_RGB s (355, 128, 128) in
  Sdlvideo.fill_rect ~rect:(Sdlvideo.rect 50 50 600 150) s c;
  Sdlvideo.flip s;
  Sdltimer.delay 20000;
=======
let main () =
  Sdl.init [];
  Graphics.init ();
  let state = State.init 10 20 1 in
  Graphics.render state;
  Sdltimer.delay(10000);
>>>>>>> 07edeae0bd71e35d142c4bc5a408bc94ce847b87
  Sdl.quit ()

let () = main ()
