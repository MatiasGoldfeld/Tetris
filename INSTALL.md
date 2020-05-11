# Installation Guidelines
Using whichever package manager you prefer, install `sdl2`, `sdl2_mixer`, `sdl2_ttf`, and `sdl2_image`. 
Note: These may have different names depending on which OS you are currently using.

Then run `opam install tsdl tsdl-mixer tsdl-ttf tsdl-image extlib lwt lwt_ppx`.

If `tsdl` fails to install or compile, try running `opam pin add tsdl.0.9.6 --dev` and then installing the other parts. 

IMPORTANT: Try the konami code while in game after starting :)

up up down down left right left right b a
