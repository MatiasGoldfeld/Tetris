(** Temporary main for testing purposes *)

let main () =
  Sdl.init [];
  Graphics.init ();
  begin try Game.init 1 with
      Game.Quit -> print_endline "Game quit unexpectedly"
  end;
  Sdl.quit ()

let () = main ()
