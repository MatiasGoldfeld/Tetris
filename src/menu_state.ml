type input_type = Button of string | Text of string
open Str


type m_field = {
  text: string;
  valid: bool;
}

type t = {
  start_game: bool;
  multiplayer: bool;
  address: string;
  is_host: bool;
  buttons: (string*button) list;
  multiplayer_fields: (string*m_field) list;
  volume: float;
  level: int;
} and button = {
    on_click: t -> t;
    button_type: input_type;
    coords: (int*int);
    dimensions: (int*int);
    selected: bool;
  }


let init_empty_button b_type = {
  on_click = (fun x -> x);
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

let b_type button = 
  match button.button_type with 
  | Button str -> str 
  | _ -> failwith "Invalid button"

let toggle_multiplayer menu = {menu with multiplayer = not menu.multiplayer}

let update_address menu address = {menu with address = String.trim address }

let address menu = menu.address

let make_button (coords:int*int) (dimensions:int*int) b_type on_click =
  {
    on_click = on_click;
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

let set_start_game menu value = 
  let start_game = begin
    let regex = Str.regexp 
        "\\b(25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9])\\.
        (25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9])\\.
        (25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9])\\.
        (25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9])\\b" 
    in
    print_endline menu.address;
    if Str.string_match regex menu.address 0 
    then value
    else false
  end in
  {menu with start_game = start_game && value}

let should_start_game menu = 
  menu.start_game


let in_button button click_coords : bool = 
  let (button_x, button_y) = button.coords in
  let (click_x, click_y) = click_coords in
  let in_x_range = button_x <= click_x 
                   && button_x+(fst button.dimensions) >= click_x in
  let in_y_range = button_y <= click_y 
                   && button_y+(snd button.dimensions) >= click_y in
  in_x_range && in_y_range

let is_multiplayer menu = menu.multiplayer

let is_host menu = menu.is_host

let toggle_host menu = {menu with is_host = not menu.is_host}

let adjust_music menu delta = {menu with volume = menu.volume +. delta}

let volume menu = menu.volume

let button_selected menu label =
  let button = List.assoc label menu.buttons in
  button.selected

let mouse_clicked menu click_coords =
  List.fold_left (fun  menu (label, button) ->
      if in_button button click_coords 
      then button.on_click menu
      else menu
    ) menu menu.buttons

let init labels = 
  let mp_fields = [init_empty_text "Host"; init_empty_text "Address"] in
  {
    start_game = false;
    multiplayer = false;
    address = "";
    is_host = false;
    buttons = List.map 
        (fun (label, b_type, on_click) -> 
           (label, make_button (0,0) (0,0) b_type on_click))
        labels;
    multiplayer_fields = mp_fields;
    volume = 0.05;
    level = 0;
  }