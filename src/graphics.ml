
let init () : unit =
  Sdl.init_subsystem [`VIDEO];
  ignore (Sdlvideo.set_video_mode 4000 4000 [`RESIZABLE])

(** [draw_tetromino piece rot surf x y size] draws [piece] at [rot] in [surface]
    with coordinates [(x, y)] and a tile length of [size].*)
let draw_tetromino (piece:Tetromino.t) (rot:int) (surf:Sdlvideo.surface)
    (x:int) (y:int) (size:int) : unit =
  ()


(** [draw_playfield state size] is the rendered playfield of [state] with a 
    tile length of [size]. *)
let draw_playfield (state:State.t) (size:int) : Sdlvideo.surface =
  let rows = State.field_height state in
  let cols = State.field_width state in
  let width = cols * size in
  let height = rows * size in
  let surf = Sdlvideo.create_RGB_surface_format
      (Sdlvideo.get_video_surface ()) [] width height in

  (* Draw every tile of every row and column *)
  for row = 0 to (rows-1) do
    for col = 0 to (cols-1) do
      let rect = Sdlvideo.rect (col * size) (row * size) size size in
      match State.value state col row with
      | State.Static color | State.Falling color->
        Sdlvideo.fill_rect ~rect:rect surf (Sdlvideo.map_RGB surf color);
      | State.Ghost _ | State.Empty -> ()
    done
  done;

  (* Draw tile borders for all rows and then all columns *)
  let border = 2 in
  let border_color = Sdlvideo.map_RGB surf (200, 200, 200) in
  for row = 0 to rows + 1 do
    let rect = Sdlvideo.rect 0 (row * size - border) width (2 * border) in
    Sdlvideo.fill_rect ~rect:rect surf border_color
  done;
  for col = 0 to cols + 1 do
    let rect = Sdlvideo.rect (col * size - border) 0 (2 * border) height in
    Sdlvideo.fill_rect ~rect:rect surf border_color
  done;

  surf

(** [draw_queue state size n] is the rendered queue of [state] with a 
    tile length of [size]. Only the first [n] tetronimoes are drawn.
    Precondition: 0 <= [n] <= [List.length Tetromino.defaults] *)
let draw_queue (state:State.t) (size:int) (n:int) : Sdlvideo.surface =
  let border = size in
  let width = Tetromino.max_size * size + 2 * border in
  let height = (State.field_height state) * size in
  let surf = Sdlvideo.create_RGB_surface_format
      (Sdlvideo.get_video_surface ()) [] width height in

  surf

let render (state:State.t) : unit =
  let screen = Sdlvideo.get_video_surface () in
  let playfield = draw_playfield state 50 in
  (* let queue = draw_queue state 50 7 in *)
  Sdlvideo.blit_surface playfield screen ();
  (* Sdlvideo.blit_surface ~src:queue ~dst:screen
     ~dst_rect:(Sdlvideo.rect (Sdlvideo.surface_info playfield).w 0 0 0) (); *)
  Sdlvideo.flip screen;
