# Installation Guidelines
Using whichever package manager you prefer, install `sdl2`, `sdl2_mixer`, and `sdl2_ttf`.

Then run `opam install tsdl tsdl-mixer tsdl-ttf`.

If `tsdl` fails to install or compile, try running `opam pin add tsdl.0.9.6 --dev` and then installing the other two parts. 
