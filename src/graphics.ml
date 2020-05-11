open Tsdl
open Tsdl_ttf
open Tsdl_image

type color = int * int * int
type duck_image = (color * Tsdl.Sdl.texture)

type t = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  pixel_format : Sdl.pixel_format;
  duck_mode : bool;
  duck_images : duck_image list;
  bg_color : color;
  font : Ttf.font;
}

let toggle_duck (ctx:t) = {
  ctx with duck_mode = not ctx.duck_mode
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

(** [render_spare rect ctx ?a color] is unit with the byproduct of rendering
    [rect] in [ctx] with the color or duck associated with [color]. *)
let render_square (rect:Sdl.rect) (ctx:t) ?(a:int=255) (color:color) : unit =
  if ctx.duck_mode then
    let texture = (List.assoc color ctx.duck_images)
    in render_duck_image ctx rect texture
  else begin
    set_color color ~a:a ctx;
    fill_rect rect ctx
  end

(** [fill_coords x y width height ctx] fills a rectangle using [ctx] at
    coordinates [(x, y)] with size [width] by [height]. *)
let fill_coords (x:int) (y:int) (w:int) (h:int) (ctx:t) : unit = 
  let rect = Sdl.Rect.create x y w h in fill_rect rect ctx

(** [create_duck_texture renderer] is the texture of the duck from [renderer].*)
let create_duck_texture (renderer:Sdl.renderer) 
    (image_path:string) : Tsdl.Sdl.texture= 
  let surface = (Tsdl_image.Image.load image_path) 
                |> unpack "failed to load image" in
  Sdl.create_texture_from_surface renderer surface 
  |> unpack "failed to make texture"

(** [duck_path_dict path renderer colors count] is the list of duck images
    from [path] associated with the colors in [colors]. *)
let rec duck_path_dict
    (path: string)
    (renderer: Sdl.renderer)
    (colors: color list)
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
    bg_color = (0, 0, 0);
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

(** [render_title ctx title x y] is unit with byproduct of rendering the title
    [title] with [ctx] positioned according to [x] and [y]. *)
let render_title (ctx:t) (title: string) (x:int) (y:int) = begin
  let bg = Sdl.Color.create 100 100 100 0 in
  let fg = Sdl.Color.create 200 200 200 0 in
  let w_desire, h_desire = 24, 20 in
  let w_true, h_true = Sdl.get_window_size ctx.window in
  let size = min (w_true / w_desire) (h_true / h_desire) in
  let text = title in
  let surf = Ttf.render_text_solid ctx.font text fg
             |> unpack "Failed to render TTF" in
  let texture = surf
                |> Sdl.create_texture_from_surface ctx.renderer
                |> unpack "Failed to create texture from font surface" in
  let _, _, (title_w, title_h) = Sdl.query_texture texture
                                 |> unpack "Failed to query texture" in
  let title_w, title_h = title_w * size / 60, title_h * size / 60 in
  let x_offset = x/2+(title_w/4) in
  draw_text ctx size text fg bg (x+x_offset, y);
end

(** [render_button ctx x y w h border selected] is unit with byproduct of 
    rendering a button with [ctx] according to the parameters [x], [y], [w], and 
    [h], with border [border] and state [selected]. *)
let render_button (ctx:t) x y w h (border:int) border_color button_color 
    selected = begin
  set_color (0, 0, 0) ctx; 
  let outline = Sdl.Rect.create (x-(border)/2) (y-(border)/2) (w+border) 
      (h+border) in
  fill_rect outline ctx;
  set_color button_color ctx; 
  let rect = Sdl.Rect.create x y w h in
  fill_rect rect ctx;
end

(** The module that is equivalent to a Menu_state. *)
module M = Menu_state

let render_field ctx menu (x,y) (w,h) label = begin
  let bg = Sdl.Color.create 100 100 100 255 in
  let fg = Sdl.Color.create 200 200 200 255 in
  draw_text ctx 18 label bg fg (x,y);
  if Menu_state.selected_text_field menu = label then begin
    set_color (0, 0, 0) ctx; 
    let border = 5 in
    let outline = Sdl.Rect.create (x-(border)/2) (y-(border)/2) (w+border) 
        (h+border) in
    fill_rect outline ctx;
  end;
  set_color (255, 255, 255) ctx; 
  let rect = Sdl.Rect.create x y w h in begin
    Sdl.set_text_input_rect (Some rect);
    fill_rect rect ctx;
    let text = Menu_state.text menu label in 
    if text <> "" then
      let bg = Sdl.Color.create 100 100 100 255 in
      let fg = Sdl.Color.create 200 200 200 255 in
      draw_text ctx 18 text bg fg (x,y);
      Sdl.start_text_input();
  end
end

let render_action_button ctx menu (x, y) (w, h) (label:string) selected = begin
  let border_color = (68,53,91) in
  let button_color = (68,53,91) in
  render_button ctx x y w h 2 border_color button_color selected;
  let bg = Sdl.Color.create 100 100 100 255 in
  let fg = Sdl.Color.create 200 200 200 255 in
  draw_text ctx 18 label bg fg (x,(y));
  (M.get_button menu label |> M.update_button (x,y) (w,h), 30)
end

let render_checkbox_button ctx menu (x, y) (w, h) (label:string) selected = begin
  let border_color = (0,0,0) in
  let button_color = (255,255,255) in
  render_button ctx x y w h 2 border_color button_color selected;
  let bg = Sdl.Color.create 100 100 100 255 in
  let fg = Sdl.Color.create 200 200 200 255 in
  draw_text ctx 18 label bg fg (x+(w*2),(y-9));
  if Menu_state.is_multiplayer menu then begin
    if label = "Multiplayer" then begin
      draw_text ctx 16 "X" bg fg (x,y);
      (M.get_button menu label |> M.update_button (x,y) (w,h), 30)
    end
    else if label = "Host game?"  then begin
      if Menu_state.is_host menu then
        draw_text ctx 16 "X" bg fg (x,y);
      render_field ctx menu (300,420) (200,30) "Address";
      (M.get_button menu label |> M.update_button (x,y) (w,h), 80)
    end
    else 
      (M.get_button menu label |> M.update_button (x,y) (w,h), 80)
  end
  else
    (M.get_button menu label |> M.update_button (x,y) (w,h), 30)
end


let rendered_button ctx button height (x,y) menu_r label= 
  let updated_button_info = begin
    match Menu_state.b_type button with 
    | "checkbox" -> 
      let selected = M.button_selected !menu_r label in
      render_checkbox_button ctx !menu_r 
        (x, !height) (20, 20) label selected
    | "action" ->
      let selected = M.button_selected !menu_r label in
      render_action_button ctx !menu_r 
        (x, !height) (300, 40) label selected
    | _ -> failwith "Invalid button string type"
  end in
  let updated_button = fst updated_button_info in 
  ((label, updated_button), snd updated_button_info)


let render_buttons (ctx:t) (menu:M.t) (coords:int*int) : M.t =
  let (x, y) = coords in
  let x_offset = x / 2 in
  let buttons = M.buttons menu in
  let height = ref y in
  let menu_r = ref menu in 
  let updated_buttons = List.mapi (fun i (label, button) -> 
      let (b, h) = rendered_button ctx button height (x+x_offset,y) menu_r label
      in height := !height + h; b
    ) buttons in begin
    Sdl.render_present ctx.renderer;
    menu_r := M.update_buttons !menu_r updated_buttons;
  end;
  !menu_r

let render_scores ctx (scores: Highscores.t list) =
  List.iteri (fun i (score, username) ->
      let bg = Sdl.Color.create 100 100 100 255 in
      let fg = Sdl.Color.create 200 200 200 255 in
      draw_text ctx 18 username bg fg (50,40*i);
      draw_text ctx 18 username bg fg (150,40*i);
    ) scores

let render_menu (ctx:t) (menu:M.t) : M.t = begin
  if Menu_state.leaderboard_mode menu then
    let scores = Highscores.high_score_getter () in
    render_scores ctx scores; menu
  else begin
    set_color (178, 249, 255) ctx; 
    Sdl.render_clear ctx.renderer |> unpack "Failed to clear renderer";
    let (w,h) = Sdl.get_window_size ctx.window in
    let menu_w = w/2 in
    let menu_h = h/2 in
    let x = menu_w/2 in
    let y = menu_h/2 in
    let rect = Sdl.Rect.create x y menu_w menu_h in
    fill_rect rect ctx;
    render_title ctx "DUCKTRIS" x y;
    render_field ctx menu (300,300) (200,30) "Username";
    render_buttons ctx menu (x,y+150)
  end
end

(** [menu_maker ctx items pos] renders a menu consisting of [items], where
    the strings are the names and the booleans are if they're higlighted, at
    coordinates [pos] with size [size] using [ctx]. *)
let menu_maker (ctx:t) (items:(string * bool) list) ((x,y):int * int)
    (size:int) : unit =
  let width, height = size * 10, size * (2 + 2 * List.length items) in
  let bg = Sdl.Color.create 127 127 127 255 in
  let fg_off = Sdl.Color.create 15 15 15 255 in
  let fg_on = Sdl.Color.create 200 15 15 255 in
  set_color (127, 127, 127) ctx;
  fill_coords (x - width / 2) (y - height / 2) width height ctx;
  let draw_item n (text, on) =
    let fg = if on then fg_on else fg_off in
    let pos = x - width / 2 + size * 2, y - height / 2 + size * (1 + 2 * n) 
    in draw_text ctx size text fg bg pos
  in List.iteri draw_item items

module type GameRenderer = sig
  module S : State.S
  val render : t -> S.t list -> (string * bool) list -> unit
end

module MakeGameRenderer (S : State.S) = struct
  module S = S

  (**   [draw_playfield ctx state pos size] renders the playfield of [state]
        at [pos]with a tile length of [size] in graphics context [ctx]. *)
  let draw_playfield (ctx:t) (state:S.t) (x,y:int*int) (size:int) : unit =
    let rows = S.field_height state in
    let cols = S.field_width state in
    let width = cols * size in
    let height = rows * size in

    (* Draw every tile of every row and column *)
    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        let rect = Sdl.Rect.create (x + col * size) (y + row * size) size size 
        in match S.value state col row with
        | State.Static color ->
          render_square rect ctx color
        | State.Falling (color, a) ->
          render_square rect ctx ~a:a color
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
  let draw_queue (ctx:t) (state:S.t) (size:int) (n:int) (x,y:int*int) : unit =
    let border = size in
    let rec draw_next pos = function
      | [] -> failwith "Missing tetromino while drawing queue"
      | piece::t ->
        draw_tetromino ctx piece 0
          (x + border) (y + border + size * 4 * pos) size;
        if pos + 1 < n
        then draw_next (pos + 1) t
        else ()
    in draw_next 0 (S.queue state)

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

  (** [draw_game_info ctx state size pos] renders the game information of 
      [state] with size [size] at coordinates [pos]. *)
  let draw_game_info (ctx:t) (state:S.t) (size:int) (x,y:int*int) : unit =
    let bg = let r, g, b = ctx.bg_color in Sdl.Color.create r g b 255 in
    let fg = Sdl.Color.create 200 200 200 255 in
    let t_score = "Score: " ^ Int.to_string (S.score state) in
    let t_time  = "Time: " ^ "idk" in
    let t_lines = "Lines: " ^ Int.to_string (S.lines state) in
    let t_level = "Level: " ^ Int.to_string (S.level state) in
    let x_offset, y_offset = size, size * 8 in
    let f_border = size / 10 in
    let f_x, f_y, f_w, f_h =
      x + size * 3 / 2, y + size * 3 / 2, size * 5, size * 5 in
    let f_out = Sdl.Rect.create (f_x - f_border) (f_y - f_border)
        (f_w + f_border * 2) (f_h + f_border * 2) in
    let f_in = Sdl.Rect.create f_x f_y f_w f_h in
    set_color (31, 31, 31) ctx; fill_rect f_out ctx;
    set_color (100, 100, 100) ctx; fill_rect f_in ctx;
    begin match S.held state with
      | None -> ()
      | Some piece ->
        draw_tetromino ctx piece 0 (x + size * 2) (y + size * 2) size
    end;
    draw_text ctx size t_score fg bg (x + x_offset, y + y_offset);
    draw_text ctx size t_time  fg bg (x + x_offset, y + y_offset + size * 2);
    draw_text ctx size t_lines fg bg (x + x_offset, y + y_offset + size * 4);
    draw_text ctx size t_level fg bg (x + x_offset, y + y_offset + size * 6)

  let render (ctx:t) (states:S.t list) (menu:(string * bool) list) : unit =
    let state = List.hd states in
    let w_desire, h_desire = 24, 20 in
    let w_true, h_true = Sdl.get_window_size ctx.window in
    let size = min (w_true / w_desire) (h_true / h_desire) in
    let x_offset = (w_true - w_desire * size + 1) / 2 in
    let y_offset = (h_true - h_desire * size + 1) / 2 in
    let field_width = S.field_width state * size in
    set_color ctx.bg_color ctx;
    Sdl.render_clear ctx.renderer |> unpack "Failed to clear renderer";
    draw_game_info ctx state size (x_offset, y_offset);
    draw_playfield ctx state (x_offset + 8 * size, y_offset) size;
    draw_queue ctx state size 5 (x_offset + field_width + 8 * size, y_offset);
    if menu = [] then () else
      menu_maker ctx menu (x_offset + size * 13, y_offset + size * 10) size;
    Sdl.render_present ctx.renderer;
end