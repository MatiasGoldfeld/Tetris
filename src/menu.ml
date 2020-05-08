

type button = {
  label: string;
  coords: (int*int);
  dimensions: (int*int);
  selected: bool;
}

type t = {
  multiplayer: bool;
  buttons: button list;
  volume: float;
  level: int;
}

let init_empty_button () = {
  label= "";
  coords= (0,0);
  dimensions = (0,0);
  selected= false;
}

let init () = {
  multiplayer= false;
  buttons= [];
  volume= 0.05;
  level= 0
}

let make_button (label:string) 
    (coords:int*int) (dimensions:int*int) =
  {
    label = label;
    coords = coords;
    dimensions = dimensions;
    selected = false;
  }

let set_multiplayer_buttons (menu:t) (buttons:button list) =
  {menu with buttons = buttons}

let in_button button click_coords : bool = 
  let (button_x, button_y) = button.coords in
  let (click_x, click_y) = click_coords in
  let in_x_range = button_x <= click_x 
                   && button_x+(fst button.dimensions) >= click_x in
  let in_y_range = button_y <= click_y 
                   && button_y+(snd button.dimensions) >= click_y in
  in_x_range && in_y_range

let mouse_clicked menu click_coords =
  { menu with buttons = List.map (fun button -> 
        if in_button button click_coords then begin
          print_endline button.label;
          {button with selected= not button.selected}
        end
        else button) 
        menu.buttons 
  }



