type event = 
  | Rotate 
  | Locking
  | Movement
  | LineClear
  | EndGame

type color = int * int * int

exception InvalidCoordinates of string

type v =
  | Empty
  | Falling of color * int
  | Static of color
  | Ghost of color * int

module type S = sig
  type t
  exception Gameover of t
  val pauseable : bool
  val init : int -> int -> int -> t
  val score : t -> int
  val level : t -> int
  val lines : t -> int
  val field_width : t -> int
  val field_height : t -> int
  val value : t -> int -> int -> v
  val queue : t -> Tetromino.t list
  val held : t -> Tetromino.t option
  val update : t -> int -> bool -> t
  val rotate : [`CCW | `CW] -> t -> t
  val move : [`Left | `Right] -> t -> t
  val hold : t -> t
  val hard_drop : t -> t
  val handle_events : (event -> unit) -> t -> t
end

module Local = struct
  (* the [i]th element in score_multipliers corresponds to the points 
     rewarded for clearing i lines *)
  let score_multipliers = [|0;100; 300; 500; 800|]

  type t = {
    score : int;
    lines : int;
    level : int;
    fall_speed : int;
    step_delta : int;
    ext_placement_move_count : int;
    ext_placement_delta : int;
    min_row : int;
    events : event list;
    queue : Tetromino.t list;
    held : Tetromino.t option;
    held_before : bool;
    falling : Tetromino.t;
    falling_rot : int;
    (* The (c, r) position of the falling tetromino. *)
    falling_pos : int * int;
    ghost_row : int;
    (* The array of rows, with 0 representing the top row. The columns are
       arrays of color options, with 0 representing the left column. Only blocks
       already placed on the playfield are represented here. *)
    playfield : color option array array
  }

  exception Gameover of t



  let pauseable = true

  let field_width (state:t) : int =
    Array.length state.playfield.(0)

  let field_height (state:t) : int =
    Array.length state.playfield

  let score (state:t) : int =
    state.score

  let level (state:t) : int =
    state.level

  let lines (state:t) : int =
    state.lines

  (** [legal state falling falling_rot falling_pos] is true if the anticipated
      movement [falling] [falling_rot] [falling_pos] is legal. *)
  let legal state falling falling_rot falling_pos =
    let rec check_row col row =
      col >= Tetromino.size falling || begin
        match Tetromino.value falling falling_rot col row with
        | None -> true
        | Some x -> 
          let abs_col = fst falling_pos + col in
          let abs_row = snd falling_pos + row in
          abs_col >= 0 && abs_col < field_width state &&
          abs_row >= 0 && abs_row < field_height state &&
          state.playfield.(abs_row).(abs_col) = None
      end && check_row (col + 1) row
    in
    let rec check_playfield col row =
      row >= Tetromino.size falling || check_row col row &&
                                       check_playfield col (row + 1)
    in check_playfield 0 0

  (** [shuffle lst] is [lst] with its elements randomly shuffled. *)
  let shuffle (lst:'a list) : 'a list =
    lst
    |> List.map (fun x -> (Random.bits (), x))
    |> List.sort compare
    |> List.map snd

  (** [next_piece state] returns the next piece of the queue in [state], along
      with an updated state. *)
  let next_piece (state:t) : t =
    let queue =
      if List.length state.queue - 1 < List.length Tetromino.defaults then
        state.queue @ shuffle Tetromino.defaults
      else
        state.queue
    in 
    {
      state with queue = List.tl queue; 
                 falling = List.hd queue; 
                 falling_rot = 0;
                 held_before = false
    }

  (** [update_ghost state] is [state] with the falling tetromino's ghost row
      updates. *)
  let rec update_ghost (state:t) : t =
    let col, start_row = state.falling_pos in
    let rec helper row =
      if legal state state.falling state.falling_rot (col, row)
      then helper (row + 1)
      else { state with ghost_row = row - 1 }
    in helper (start_row + 1)


  let drop_help state =
    let column = 5 - (Tetromino.size state.falling / 2) in
    if legal state state.falling state.falling_rot (column, 0)
    then {state with 
          step_delta = 0;
          falling_pos = (column, 0);
          ext_placement_move_count = 0;
          ext_placement_delta = 0;
          min_row = 0} |> update_ghost
    else begin
      if legal state state.falling state.falling_rot (column, -1)
      then {state with 
            step_delta = 0;
            falling_pos = (column, -1);
            ext_placement_move_count = 0; 
            ext_placement_delta = 0;
            min_row = -1} |> update_ghost
      else raise (Gameover {state with events = EndGame::state.events})
    end

  (** [drop piece state] is the [state] with a new piece initialized as the 
      falling piece on the top of the playfield. *)
  let drop (state:t) : t = 
    let new_state = next_piece state in
    drop_help new_state

  (** [recalculate_fall_speed state] is [state] with the fall speed adjusted to
      current level. *)
  let recalculate_fall_speed (state:t) : t =
    let level_f = state.level - 1
                  |> max 0
                  |> min 14
                  |> Int.to_float
    in
    let fall_speed = 1000. *. (0.8 -. (level_f *. 0.007)) ** level_f in
    { state with fall_speed = Float.to_int fall_speed }


  let init (width:int) (height:int) (level:int) : t =
    let queue = shuffle Tetromino.defaults in
    {
      score = 0;
      lines = 0;
      level = level;
      fall_speed = -1;
      step_delta = 0;
      ext_placement_move_count = 0;
      ext_placement_delta = 0;
      min_row = -1;
      events = [];
      queue = queue;
      held = None;
      held_before = false;
      falling = List.hd queue;
      falling_rot = -1;
      falling_pos = -1, -1;
      ghost_row = -1;
      playfield = Array.make_matrix height width None
    }
    |> drop
    |> recalculate_fall_speed

  let row_full acc item =
    match item with
    | Some x -> true && acc
    | None -> false && acc

  let check_row array =
    Array.fold_left row_full true array

  (* Note: Could not use Array.map becaus we only want to do this for a part
     of the array. *)
  let rec fill_in_rows state height =
    if height >= 1
    then (state.playfield.(height) <- state.playfield.(height - 1);
          if height = 0 
          then Array.fill state.playfield.(height) 0 (field_width state) None
          else ();
          fill_in_rows state (height - 1))
    else state.playfield.(height) <- Array.make (field_width state) None

  let rec clear_lines_helper state height line_dif =
    if height >= 0 then begin
      if (check_row state.playfield.(height))
      then
        (fill_in_rows state height;
         let new_state = recalculate_fall_speed
             {state with lines = state.lines + 1;
                         level = max state.level (1 + (state.lines + 1) / 10)}
         in clear_lines_helper new_state height (line_dif+1))
      else clear_lines_helper state (height - 1) (line_dif)
    end
    else
      let new_score = state.score + state.level*score_multipliers.(line_dif) in
      if line_dif > 0 
      then {state with score = new_score; events = LineClear::state.events} 
      else {state with score = new_score}

  let clear_lines state = 
    clear_lines_helper state (field_height state - 1) 0

  let value (state:t) (c:int) (r:int) : v =
    if r < 0 || c < 0 || r >= field_height state || c >= field_width state
    then Empty
    else match state.playfield.(r).(c) with
      | Some color -> Static color
      | None ->
        let tet = state.falling in
        let fall_c, fall_r = state.falling_pos in
        let fall_rot = state.falling_rot in
        if c >= fall_c && c - fall_c < Tetromino.size state.falling then
          let check = Tetromino.value tet fall_rot (c - fall_c) in
          match check (r - fall_r) with
          | Some color ->
            let a = (500 - state.ext_placement_delta) * 255 / 500 in
            Falling (color, a)
          | None ->
            match check (r - state.ghost_row) with
            | Some color -> Ghost (color, 95)
            | None -> Empty
        else
          Empty

  (** [is_t_spin state pos_x pos_y ] checks if [state]'s falling tetromino 
      satisfies the t_spin conditions *)
  let is_mini_t_spin state pos_x pos_y =
    let tet = state.falling in
    if Tetromino.size tet <> 3 then false
    else begin
      let uLeft = value state (pos_y) (pos_x) in
      let uRight = value state (pos_y+2) (pos_x) in
      let dLeft = value state (pos_y) (pos_x) in
      let dRight = value state (pos_y) (pos_x+2) in
      match uLeft, uRight, dLeft, dRight with
      | _, Static _, Static _, Static _ 
      | Static _, _, Static _, Static _
      | Static _, Static _, _, Static _
      | Static _, Static _, Static _, _ -> true
      | _ -> false
    end

  let place_piece state pos_x pos_y =
    let spin_score_state =
      if is_mini_t_spin state pos_x pos_y then 
        {state with score = state.score+100*state.level} else
        state in 
    for col = pos_x to pos_x + (Tetromino.size state.falling - 1) do
      for row = pos_y to pos_y + (Tetromino.size state.falling - 1) do 
        let new_val = Tetromino.value state.falling state.falling_rot 
            (col - pos_x) (row - pos_y) in
        if new_val <> None && (row - pos_y >= 0)
        then 
          state.playfield.(row).(col) <- new_val
        else ()
      done
    done;
    let new_state = {spin_score_state with events = Locking::state.events} in
    drop (clear_lines new_state)

  let queue (state:t) : Tetromino.t list =
    state.queue

  let held (state:t) : Tetromino.t option =
    state.held


  (** [step state] is the [state] after the falling piece has stepped down. *)
  let step (state:t) : t =
    let pos_x, pos_y = state.falling_pos in
    let ext_adjust = if pos_y + 1 > state.min_row then 0 else 1
    in { 
      state with falling_pos = (pos_x, pos_y + 1); step_delta = 0; 
                 ext_placement_delta = state.ext_placement_delta * ext_adjust; 
                 ext_placement_move_count = 
                   state.ext_placement_move_count * ext_adjust;
                 min_row = pos_y + 1
    }

  let update (state:t) (delta:int) (soft_drop:bool) : t =
    let new_delta = state.step_delta + delta * if soft_drop then 20 else 1 in
    let new_ext_delta = state.ext_placement_delta + delta in
    let state = {state with step_delta = new_delta} in
    if state.step_delta >= state.fall_speed 
    then begin
      if snd state.falling_pos <> state.ghost_row
      then step {state with score = 
                              if soft_drop then state.score + 1 
                              else state.score } 
      else begin 
        if new_ext_delta >= 500
        then place_piece state (fst state.falling_pos) (snd state.falling_pos)
        else {state with ext_placement_delta = new_ext_delta}
      end
    end
    else state

  let ext_placement_add state = 
    let move_count_add =
      if snd state.falling_pos = state.ghost_row then 1 else 0 in
    if state.ext_placement_move_count >= 15
    then 
      {
        state with 
        ext_placement_move_count = 
          state.ext_placement_move_count + move_count_add
      }
    else 
      {
        state with 
        ext_placement_move_count = 
          state.ext_placement_move_count + move_count_add;
        ext_placement_delta = 0
      }

  let rotate (rotation:[`CCW | `CW]) (state:t) : t =
    let rot = state.falling_rot in
    let new_rot = (rot + match rotation with `CCW -> 3 | `CW -> 1) mod 4 in
    let rec test_rot = function
      | [] -> state
      | (x, y)::t ->
        let check_pos =
          (x + fst state.falling_pos, y + snd state.falling_pos) in
        if legal state state.falling new_rot check_pos then
          let ext_state = ext_placement_add state in
          { ext_state with falling_rot = new_rot; falling_pos = check_pos;
                           events = Rotate::state.events}
          |> update_ghost
        else test_rot t
    in test_rot (Tetromino.wall_kicks state.falling rot rotation)

  let move (direction:[`Left | `Right]) (state:t) : t =
    let new_pos = match direction with
      | `Left -> fst state.falling_pos - 1, snd state.falling_pos
      | `Right -> fst state.falling_pos + 1, snd state.falling_pos
    in 
    if legal state state.falling state.falling_rot new_pos then 
      let ext_state = ext_placement_add state in
      { ext_state with falling_pos = new_pos; events = Movement::state.events}
      |> update_ghost
    else state

  (** Comment up!*)
  let hold_drop state piece =
    let new_state = {state with falling = piece; falling_rot = 0} in
    drop_help new_state

  let hold (state:t) : t =
    match held state with
    | None ->
      let this_piece = state.falling in
      let new_state = drop {state with held = Some this_piece} in
      {new_state with held_before = true}
    | Some piece ->
      if state.held_before 
      then state
      else hold_drop
          {state with held = Some state.falling; held_before = true} piece

  let hard_drop (state:t) : t =
    place_piece 
      {state with score = (state.score + 2*(state.ghost_row - (snd state.falling_pos)))} 
      (fst state.falling_pos) state.ghost_row

  let handle_events (f:event -> unit) (state:t) : t =
    List.iter f (List.rev state.events);
    { state with events = [] }

end


let make_test_state scoret linest levelt fall_speedt step_deltat 
    ext_placement_move_countt ext_placement_deltat min_rowt eventst queuet 
    heldt held_beforet fallingt falling_rott falling_post ghost_rowt 
    playfieldt : Local.t = 
  {
    score = scoret;
    lines = linest;
    level = levelt;
    fall_speed = fall_speedt;
    step_delta = step_deltat;
    ext_placement_move_count = ext_placement_move_countt;
    ext_placement_delta = ext_placement_deltat;
    min_row = min_rowt;
    events = eventst;
    queue = queuet;
    held = heldt;
    held_before = held_beforet;
    falling = fallingt;
    falling_rot = falling_rott;
    falling_pos = falling_post;
    ghost_row = ghost_rowt;
    playfield = playfieldt
  }

