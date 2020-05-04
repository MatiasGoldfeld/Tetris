open Tsdl

type t = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  pixel_format : Sdl.pixel_format;
}

(** [unpack message result] is [value] if [result] is [Ok value]. Otherwise, it
    logs [message] along with the error, and exits the program. *)
let unpack (message:string) (result:'a Sdl.result) : 'a =
  match result with
  | Error (`Msg e) -> Sdl.log "%s: %s" message e; Sdl.quit (); exit 1
  | Ok value -> value

(** [set_color color alpha ctx] sets the render color of [ctx] to [color] and
    [alpha], with a default of 255 (opaque). *)
let set_color (r, g, b:int * int * int) ?(a:int=255) (ctx:t): unit =
  Sdl.set_render_draw_color ctx.renderer r g b a |> ignore

(** [fill_rect rect ctx] fills [rect] using [ctx]. *)
let fill_rect (rect:Sdl.rect) (ctx:t) : unit = 
  Sdl.render_fill_rect ctx.renderer (Some rect) |> ignore

(** [fill_coords x y width height ctx] fills a rectangle using [ctx] at
    coordinates [(x, y)] with size [width] by [height]. *)
let fill_coords (x:int) (y:int) (w:int) (h:int) (ctx:t) : unit = 
  let rect = Sdl.Rect.create x y w h in fill_rect rect ctx

let init () : t =
  Sdl.init_sub_system Sdl.Init.video |> unpack "Graphics init error";
  let window =
    Sdl.create_window ~w:1000 ~h:1500 "Ducktris" Sdl.Window.opengl
    |> unpack "Create window error" in
  let pixel_format =
    Sdl.alloc_format (Sdl.get_window_pixel_format window)
    |> unpack "Alloc format error" in
  let renderer =
    Sdl.create_renderer window
    |> unpack "Create renderer error" in
  Sdl.(set_render_draw_blend_mode renderer Blend.mode_blend)
  |> unpack "Error setting renderer blend mode";
  {
    window = window;
    renderer = renderer;
    pixel_format = pixel_format;
  }

(** [draw_tetromino ctx piece rot surf x y size] draws [piece] at [rot] in
    [surface] with coordinates [(x, y)] and a tile length of [size] in graphics
    context [ctx].*)
let draw_tetromino (ctx:t) (piece:Tetromino.t) (rot:int) (surf:Sdl.surface)
    (x:int) (y:int) (size:int) : unit =
  ()


(** [draw_playfield ctx state pos size] is the rendered playfield of [state]
    at [pos]with a tile length of [size] in graphics context [ctx]. *)
let draw_playfield (ctx:t) (state:State.t) (x,y:int*int) (size:int) : unit =
  let rows = State.field_height state in
  let cols = State.field_width state in
  let width = cols * size in
  let height = rows * size in

  (* Draw every tile of every row and column *)
  for row = 0 to (rows-1) do
    for col = 0 to (cols-1) do
      let rect = Sdl.Rect.create (x + col * size) (y + row * size) size size in
      match State.value state col row with
      | State.Static color ->
        set_color color ctx;
        fill_rect rect ctx
      | State.Falling (color, a) | State.Ghost (color, a) ->
        set_color color ~a:a ctx;
        fill_rect rect ctx
      | State.Empty -> ()
    done
  done;

  (* Draw tile borders for all rows and then all columns *)
  let border = 2 in
  set_color (200, 200, 200) ~a:255 ctx;
  for row = 0 to rows do
    fill_coords x (y + row * size - border) width (2 * border) ctx
  done;
  for col = 0 to cols do
    fill_coords (x + col * size - border) y (2 * border) height ctx
  done

(** [draw_queue ctx state size n] is the rendered queue of [state] with a 
    tile length of [size] in graphics context [ctx]. Only the first [n]
    tetronimoes are drawn.
    Requires: 0 <= [n] <= [List.length Tetromino.defaults] *)
let draw_queue (ctx:t) (state:State.t) (size:int) (n:int) : unit =
  let border = size in
  let width = Tetromino.max_size * size + 2 * border in
  let height = (State.field_height state) * size in
  ()

let render (ctx:t) (state:State.t) : unit =
  set_color (80, 80, 80) ctx;
  Sdl.render_clear ctx.renderer |> unpack "Failed to clear renderer";
  draw_playfield ctx state (0, 0) 70;
  Sdl.render_present ctx.renderer
