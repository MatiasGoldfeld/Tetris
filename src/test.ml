open Audio

let test =
  Sdl.init [`AUDIO];
  let audio = init "./resources/" in
  start_music audio;
  Sdltimer.delay(1000);
