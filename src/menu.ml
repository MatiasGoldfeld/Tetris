type input_type = Button of string | Text of string

type button = {
  on_click: unit -> unit;
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
}

let init_empty_button b_type = {
  on_click = (fun x -> x );
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

let init labels = 
  let multiplayer_fields = 
    [init_empty_text "Address"] in
  {
    buttons = List.map 
        (fun (label, b_type) -> 
           (label, init_empty_button b_type)
        )
        labels;
    multiplayer_fields= multiplayer_fields;
    volume= 0.05;
    level= 0
  }

let get_button menu label = List.assoc label menu.buttons

let button_type button = button.button_type

let buttons menu = menu.buttons

let make_button 
    (coords:int*int) (dimensions:int*int) b_type on_click =
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
        if in_button button click_coords then begin
          button.on_click ();
          (label, {button with selected = not button.selected})
        end
        else (label, button))
        menu.buttons 
  }



