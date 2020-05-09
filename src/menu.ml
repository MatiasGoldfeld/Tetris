type button = {
  coords: (int*int);
  dimensions: (int*int);
  selected: bool;
}

type t = {
  multiplayer: bool;
  buttons: (string*button) list;
  volume: float;
  level: int;
}

let init_empty_button () = {
  coords= (0,0);
  dimensions = (0,0);
  selected= false;
}

let init labels = {
  multiplayer= false;
  buttons= List.map (fun label -> (label, init_empty_button())) labels;
  volume= 0.05;
  level= 0
}

let get_button menu label = List.assoc label menu.buttons

let make_button 
    (coords:int*int) (dimensions:int*int) =
  {
    coords = coords;
    dimensions = dimensions;
    selected = false;
  }

let update_button (coords:int*int) (dimensions:int*int) button =
  {
    button with coords=coords; dimensions=dimensions 
  }

let update_buttons menu buttons =
  {
    menu with buttons=buttons
  }


let set_multiplayer_buttons (menu:t) (buttons:(string*button) list) =
  {menu with buttons = buttons}

let in_button button click_coords : bool = 
  let (button_x, button_y) = button.coords in
  let (click_x, click_y) = click_coords in
  let in_x_range = button_x <= click_x 
                   && button_x+(fst button.dimensions) >= click_x in
  let in_y_range = button_y <= click_y 
                   && button_y+(snd button.dimensions) >= click_y in
  in_x_range && in_y_range

let button_selected menu label =
  try 
    let button = List.assoc label menu.buttons in
    button.selected
  with _ -> false

let mouse_clicked menu click_coords =
  { menu with buttons = List.map (fun (label, button) -> 
        if in_button button click_coords then begin
          (label, {button with selected= not button.selected})
        end
        else (label, button))
        menu.buttons 
  }



