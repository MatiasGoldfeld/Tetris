(** Temporary main for testing purposes *)
let main () =
  Sdl.init [];
  Graphics.init ();
  let state = State.init 10 20 1 in
  Graphics.render state;
  Sdltimer.delay(10000);
  Sdl.quit ()

let () = main ()
