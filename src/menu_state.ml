type input_type = Button of string | Text of string
open Str


type m_field = {
  dimensions:(int*int);
  coords: (int*int)
}

type t = {
  username: string;
  start_game: bool;
  multiplayer: bool;
  address: string;
  selected_text_field: string;
  is_host: bool;
  buttons: (string*button) list;
  text_fields: (string*m_field) list;
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
      dimensions= (0,0);
      coords= (0,0)
    })

let get_button menu label = List.assoc label menu.buttons

let buttons menu = menu.buttons

let b_type button = 
  match button.button_type with 
  | Button str -> str 
  | _ -> failwith "Invalid button"

let toggle_multiplayer menu = {menu with multiplayer = not menu.multiplayer}

let update_text menu text = 
  let label = menu.selected_text_field in
  match label with
  | "Address" -> {menu with address = String.trim text}
  | "Username" -> {menu with username = String.trim text}
  | _ -> failwith ("invalid input field: "^label)

let address menu = menu.address

let make_button (coords:int*int) (dimensions:int*int) b_type on_click =
  {
    on_click = on_click;
    button_type = Button b_type;
    coords = coords;
    dimensions = dimensions;
    selected = false;
  }

let text_fields menu = menu.text_fields

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
        (25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9])\\b
        :[1-9]\\|[1-9][0-9]\\|[1-9][0-9][0-9]\\|[1-9][0-9][0-9][0-9]" 
    in
    if Str.string_match regex menu.address 0 
    then value
    else false
  end in
  {menu with start_game = start_game && value}

let should_start_game menu = 
  menu.start_game

let text menu text_field = 
  match text_field with
  | "Address" -> menu.address
  | "Username" -> menu.username
  | _ -> failwith "No text field exists with that label."

let in_input coords dimensions click_coords : bool = 
  let (rect_x, rect_y) = coords in
  let (click_x, click_y) = click_coords in
  let in_x_range = rect_x <= click_x 
                   && rect_x+(fst dimensions) >= click_x in
  let in_y_range = rect_y <= click_y 
                   && rect_y+(snd dimensions) >= click_y in
  in_x_range && in_y_range

let is_multiplayer menu = menu.multiplayer

let is_host menu = menu.is_host

let toggle_host menu = {menu with is_host = not menu.is_host}

let adjust_music menu delta = {menu with volume = menu.volume +. delta}

let volume menu = menu.volume

let button_selected menu label =
  let button = List.assoc label menu.buttons in
  button.selected

let selected_text_field menu =
  menu.selected_text_field

let mouse_clicked menu click_coords =
  let selected_text_field = List.fold_left
      (fun selected (label, (field:m_field)) ->
         if in_input (field.coords) (field.dimensions) click_coords 
         then label
         else selected
      ) menu.selected_text_field menu.text_fields in
  let updated_menu = 
    List.fold_left (fun  menu (label, button) ->
        if in_input (button.coords) (button.dimensions) click_coords 
        then button.on_click menu
        else menu
      ) menu menu.buttons
  in { updated_menu with selected_text_field = selected_text_field}

let init labels = 
  let mp_fields = [("Username", {
      dimensions= (200,30);
      coords= (300,300)
    }); ("Address", {
      dimensions= (200,30);
      coords= (300,420)
    })] in
  {
    username = "";
    start_game = false;
    multiplayer = false;
    selected_text_field= "Username";
    address = "";
    is_host = false;
    buttons = List.map 
        (fun (label, b_type, on_click) -> 
           (label, make_button (0,0) (0,0) b_type on_click))
        labels;
    text_fields = mp_fields;
    volume = 0.05;
    level = 0;
  }