# Installation Guidelines
Using whichever package manager you prefer, install `sdl2`, `sdl2_mixer`, `sdl2_ttf`, and `sdl2_image`.

Then run `opam install tsdl tsdl-mixer tsdl-ttf tsdl-image`.

If `tsdl` fails to install or compile, try running `opam pin add tsdl.0.9.6 --dev` and then installing the other two parts. 
