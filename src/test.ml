open Audio

let test =
  Sdl.init [`AUDIO];
  let audio = init "./src" in
  start_music audio;
  Sdltimer.delay(1000);
