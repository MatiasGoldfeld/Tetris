open Tsdl

type input_type = Button of string | Text of string

type button = {
  button_type: input_type;
  coords: (int*int);
  dimensions: (int*int);
  selected: bool;
}

type m_field = {
  text: string;
  valid: bool;
}

type t = {
  buttons: (string*button) list;
  multiplayer_fields: (string*m_field) list;
  volume: float;
  level: int;
  gfx: (t -> t);
  last_update: int;
}

module type Button = sig
  type t
  val onPress : t -> t
  val coords : t -> (int*int)
end

let init_empty_button b_type = {
  button_type = Button b_type;
  coords= (0,0);
  dimensions = (0,0);
  selected= false;
}

let init_empty_text label = 
  (label, {
      text = "";
      valid= false;
    })

let get_button menu label = List.assoc label menu.buttons

let buttons menu = menu.buttons

let make_button 
    (coords:int*int) (dimensions:int*int) b_type =
  {
    button_type = Button b_type;
    coords = coords;
    dimensions = dimensions;
    selected = false;
  }

let multiplayer_fields menu = menu.multiplayer_fields

let update_button (coords:int*int) (dimensions:int*int) button =
  {
    button with coords=coords; dimensions=dimensions 
  }

let update_buttons menu buttons =
  {
    menu with buttons=buttons
  }


let in_button button click_coords : bool = 
  let (button_x, button_y) = button.coords in
  let (click_x, click_y) = click_coords in
  let in_x_range = button_x <= click_x 
                   && button_x+(fst button.dimensions) >= click_x in
  let in_y_range = button_y <= click_y 
                   && button_y+(snd button.dimensions) >= click_y in
  in_x_range && in_y_range

let button_selected menu label =
  let button = List.assoc label menu.buttons in
  button.selected

let mouse_clicked menu click_coords =
  { menu with buttons = List.map (fun (label, button) ->
        if in_button button click_coords then
          (label, {button with selected = not button.selected})
        else (label, button))
        menu.buttons 
  }

(** [handle_events] handles all SDL events by using actions from [inputs] in
    [game]. *)
let rec handle_events (menu : t) : t =
  let event = Sdl.Event.create () in
  if not (Sdl.poll_event (Some event)) then menu
  else handle_events begin
      match Sdl.Event.(enum (get event typ)) with
      | `Mouse_button_down ->
        let click_coords = (Sdl.Event.(get event mouse_button_x), 
                            Sdl.Event.(get event mouse_button_y)) in 
        mouse_clicked menu click_coords
      | `Quit ->
        Sdl.log "Quit event handled from main menu";
        Sdl.quit ();
        exit 0
      | _ -> menu
    end

let rec loop (menu : t) : unit =
  let menu = handle_events menu in
  let time = Int32.to_int (Sdl.get_ticks ()) in
  let delta = (time - menu.last_update) in
  let menu = if delta < 1000 / 60 then menu else
      menu
  in loop menu

let init gfx labels = 
  let mp_fields = [init_empty_text "Host"; init_empty_text "Address"] in
  loop {
    buttons = List.map 
        (fun (label, b_type) -> 
           (label, init_empty_button b_type)
        )
        labels;
    multiplayer_fields = mp_fields;
    volume = 0.05;
    level = 0;
    gfx = gfx;
    last_update = Int32.to_int @@ Sdl.get_ticks ();
  }