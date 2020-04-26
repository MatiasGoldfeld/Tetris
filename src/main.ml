(** Temporary main for testing purposes *)
let main () =
  Sdl.init [];
  Graphics.init ();
  let audio = Audio.init "./resources/" in
  begin try Game.init 1 audio; with
      Game.Quit -> print_endline "Game quit unexpectedly"
  end;
  Sdl.quit ()

let () = main ()
