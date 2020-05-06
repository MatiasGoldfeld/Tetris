open Tsdl
open Tsdl_ttf
open Tsdl_image

type duck_image = (State.color * Tsdl.Sdl.texture)


type t = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  pixel_format : Sdl.pixel_format;
  duck_mode: bool;
  duck_images : duck_image list;
  bg_color : State.color;
  font : Ttf.font;
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


(**  [render_duck ctx rect image_path ] draws a duck (based on the image at 
     [image_path]) on this [rect] in graphics contect [ctx] *)
let render_duck_image (ctx:t) (rect:Sdl.rect) (texture:Tsdl.Sdl.texture) =
  Sdl.render_copy ~dst:rect ctx.renderer texture  
  |> unpack "failed to render texture"

(** [fill_rect rect ctx] fills [rect] using [ctx]. *)
let fill_rect (rect:Sdl.rect) (ctx:t) : unit = 
  Sdl.render_fill_rect ctx.renderer (Some rect) |> ignore

let render_square (rect:Sdl.rect) (ctx:t) (color:State.color) : unit =
  if ctx.duck_mode then
    let texture = (List.assoc color ctx.duck_images)
    in render_duck_image ctx rect texture
  else begin
    set_color color ctx;
    fill_rect rect ctx
  end

(** [fill_coords x y width height ctx] fills a rectangle using [ctx] at
    coordinates [(x, y)] with size [width] by [height]. *)
let fill_coords (x:int) (y:int) (w:int) (h:int) (ctx:t) : unit = 
  let rect = Sdl.Rect.create x y w h in fill_rect rect ctx

let create_duck_texture (renderer:Sdl.renderer) 
    (image_path:string) : Tsdl.Sdl.texture= 
  let surface = (Tsdl_image.Image.load image_path) 
                |> unpack "failed to load image" in
  Sdl.create_texture_from_surface renderer surface 
  |> unpack "failed to make texture"

let rec duck_path_dict
    (path: string)
    (renderer: Sdl.renderer)
    (colors: State.color list)
    (count:int) : duck_image list = 
  match colors with 
  | h::t -> 
    let texture = (path^"/images/duck"^(string_of_int count)^".jpg" 
                   |> create_duck_texture renderer) in
    (h,texture)::(duck_path_dict path renderer t (count+1))
  | [] -> []

let init (duck_mode:bool) (path:string) : t =
  Sdl.init_sub_system Sdl.Init.video |> unpack "Graphics init error";
  Ttf.init () |> unpack "TTF init error";
  let window =
    Sdl.create_window ~w:800 ~h:800 "Ducktris" Sdl.Window.(opengl + resizable)
    |> unpack "Create window error" in
  let pixel_format =
    Sdl.alloc_format (Sdl.get_window_pixel_format window)
    |> unpack "Alloc format error" in
  let renderer =
    Sdl.create_renderer window
    |> unpack "Create renderer error" in
  Sdl.(set_render_draw_blend_mode renderer Blend.mode_blend)
  |> unpack "Error setting renderer blend mode";
  Sdl.set_window_minimum_size window ~w:660 ~h:600;
  {
    window = window;
    renderer = renderer;
    pixel_format = pixel_format;
    duck_mode = duck_mode;
    duck_images = duck_path_dict path renderer Tetromino.colors 1;
    bg_color = (80, 80, 80);
    font = Ttf.open_font (path ^ "fonts/PTS55F.ttf") 60
           |> unpack "Failed loading font";
  }

(** [draw_tetromino ctx piece rot x y size] draws [piece] at [rot] with
    coordinates [(x, y)] and a tile length of [size] in graphics
    context [ctx].*)
let draw_tetromino (ctx:t) (piece:Tetromino.t) (rot:int) (x:int) (y:int)
    (size:int) : unit =
  let max_size = Tetromino.max_size in
  let off = (size * (max_size - Tetromino.size piece) + 1) / 2 in
  for row = 0 to max_size - 1 do
    for col = 0 to max_size - 1 do
      match Tetromino.value piece rot col row with
      | None -> ()
      | Some color ->
        set_color color ctx;
        fill_coords (x + off + col * size) (y + off + row * size) size size ctx;
    done
  done

(** [draw_playfield ctx state pos size] renders the playfield of [state]
    at [pos]with a tile length of [size] in graphics context [ctx]. *)
let draw_playfield (ctx:t) (state:State.t) (x,y:int*int) (size:int) : unit =
  let rows = State.field_height state in
  let cols = State.field_width state in
  let width = cols * size in
  let height = rows * size in

  (* Draw every tile of every row and column *)
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let rect = Sdl.Rect.create (x + col * size) (y + row * size) size size in
      match State.value state col row with
      | State.Static color ->
        (*set_color color ctx;
          fill_rect rect ctx*)
        render_square rect ctx color
      | State.Falling (color, a) ->
        render_square rect ctx color
      | State.Ghost (color, a) ->
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

(** [draw_queue ctx state size n pos] renders the queue of [state] with a 
    tile length of [size] in graphics context [ctx] at coordinates [pos].
    Only the first [n] tetronimoes are drawn.
    Requires: 0 <= [n] <= [List.length Tetromino.defaults] *)
let draw_queue (ctx:t) (state:State.t) (size:int) (n:int) (x,y:int*int) : unit =
  let border = size in
  let rec draw_next pos = function
    | [] -> failwith "Missing tetromino while drawing queue"
    | piece::t ->
      draw_tetromino ctx piece 0
        (x + border) (y + border + size * 4 * pos) size;
      if pos + 1 < n
      then draw_next (pos + 1) t
      else ()
  in draw_next 0 (State.queue state)

(** [draw_text ctx size text fg bg pos] draws [text] with size [size] at
    coordinates [pos] using [ctx], with a foreground color of [fg] and a
    background color of [bg]. *)
let draw_text (ctx:t) (size:int) (text:string) (fg:Sdl.color) (bg:Sdl.color)
    (x,y:int*int) : unit =
  let surf = Ttf.render_text_shaded ctx.font text fg bg
             |> unpack "Failed to render TTF" in
  let texture = surf
                |> Sdl.create_texture_from_surface ctx.renderer
                |> unpack "Failed to create texture from font surface" in
  let _, _, (score_w, score_h) = Sdl.query_texture texture
                                 |> unpack "Failed to query texture" in
  let score_w, score_h = score_w * size / 60, score_h * size / 60 in
  let score_rect = Sdl.Rect.create x y score_w score_h in
  Sdl.render_copy ~dst:score_rect ctx.renderer texture
  |> unpack "Failed to copy text texture onto screen";
  Sdl.free_surface surf;
  Sdl.destroy_texture texture

(** [draw_game_info ctx state size pos] renders the game information of [state]
    with size [size] at coordinates [pos]. *)
let draw_game_info (ctx:t) (state:State.t) (size:int) (x,y:int*int) : unit =
  let bg = let r, g, b = ctx.bg_color in Sdl.Color.create r g b 255 in
  let fg = Sdl.Color.create 200 200 200 255 in
  let t_score = "Score: " ^ Int.to_string (State.score state) in
  let t_time  = "Time: " ^ "idk" in
  let t_lines = "Lines: " ^ Int.to_string (State.lines state) in
  let t_level = "Level: " ^ Int.to_string (State.level state) in
  let x_offset, y_offset = size, size * 8 in
  let f_border = size / 10 in
  let f_x, f_y, f_w, f_h =
    x + size * 3 / 2, y + size * 3 / 2, size * 5, size * 5 in
  let f_out = Sdl.Rect.create (f_x - f_border) (f_y - f_border)
      (f_w + f_border * 2) (f_h + f_border * 2) in
  let f_in = Sdl.Rect.create f_x f_y f_w f_h in
  set_color (31, 31, 31) ctx; fill_rect f_out ctx;
  set_color (100, 100, 100) ctx; fill_rect f_in ctx;
  begin match State.held state with
    | None -> ()
    | Some piece ->
      draw_tetromino ctx piece 0 (x + size * 2) (y + size * 2) size
  end;
  draw_text ctx size t_score fg bg (x + x_offset, y + y_offset);
  draw_text ctx size t_time  fg bg (x + x_offset, y + y_offset + size * 2);
  draw_text ctx size t_lines fg bg (x + x_offset, y + y_offset + size * 4);
  draw_text ctx size t_level fg bg (x + x_offset, y + y_offset + size * 6)

let render (ctx:t) (states:State.t list) : unit =
  let state = fst states in
  let w_desire, h_desire = 24, 20 in
  let w_true, h_true = Sdl.get_window_size ctx.window in
  let size = min (w_true / w_desire) (h_true / h_desire) in
  let x_offset = (w_true - w_desire * size + 1) / 2 in
  let y_offset = (h_true - h_desire * size + 1) / 2 in
  let field_width = State.field_width state * size in
  set_color ctx.bg_color ctx;
  Sdl.render_clear ctx.renderer |> unpack "Failed to clear renderer";
  let start = Sdl.get_ticks () in (* temp *)
  draw_game_info ctx state size (x_offset, y_offset);
  let time_game = Sdl.get_ticks () in (* temp *)
  draw_playfield ctx state (x_offset + 8 * size, y_offset) size;
  let time_field = Sdl.get_ticks () in (* temp *)
  draw_queue ctx state size 5 (x_offset + field_width + 8 * size, y_offset);
  let time_queue = Sdl.get_ticks () in (* temp *)
  Sdl.render_present ctx.renderer;
  (* The start variable and the following are temp perf testing code *)
  Printf.printf "Render time: %li, %li, %li"
    (Int32.sub time_game start) (Int32.sub time_field time_game) (Int32.sub time_queue time_field);
  print_newline ()
