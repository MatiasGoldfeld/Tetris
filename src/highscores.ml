type t = (int*string)

(** [get_name lst] is [lst] combined into a single, safe string. *)
let rec get_name lst = 
  match lst with
  | [] -> ""
  | h::t -> let next_word = get_name t in
    if next_word = "" 
    then (String.escaped h)^(next_word)
    else (String.escaped h)^" "^(next_word)

(** [high_score_getter_helper lst] is [lst] converted into a list of highscores.
*)
let rec high_score_getter_helper lst =
  match lst with
  | [] -> []
  | h::t -> 
    let string_list = String.split_on_char ' ' h in
    match int_of_string_opt (List.hd string_list) with
    | None -> (0,get_name (List.tl string_list))::high_score_getter_helper t
    | Some x -> (x,get_name (List.tl string_list))::high_score_getter_helper t

let high_score_getter () =
  try let channel = 
        open_in "resources/ifyouchangethisfileyouwillgetazero.txt" in
    let read_in = Std.input_list channel in
    let the_list = high_score_getter_helper read_in in
    close_in channel;
    the_list
  with Sys_error _ ->
    let make_channel = 
      open_out "resources/ifyouchangethisfileyouwillgetazero.txt" in
    close_out make_channel; []

(** [sort_scores new_score lst] is the list of sorted scores including
    [new_score]. *)
let rec sort_scores (new_score:t) (lst:t list) = 
  match lst with
  | [] -> [new_score]
  | h::t ->
    print_endline (Int.to_string (fst h));
    if fst h >= fst new_score
    then h::(sort_scores new_score t)
    else new_score::lst

(** [make_score_list highscore] is the sorted list of highscores with 
    [highscore] included in that list. *)
let make_score_list highscore =
  sort_scores highscore (high_score_getter ())

(** [write_top_counter write_to score_list counter] writes the top [counter]
    values of [score_list] to [write_to]. If [score_list] is shorter than 
    [counter], it stops writing once [score_list] is iterated through. *)
let rec write_top_counter write_to score_list counter =
  if counter <= 0
  then ()
  else
    match score_list with
    | [] -> ()
    | h::t -> Printf.fprintf write_to "%s\n" 
                (Int.to_string (fst h) ^ " " ^ String.escaped (snd h));
      write_top_counter write_to t (counter - 1)

let write_high_score highscore = 
  let score_list = make_score_list highscore in
  let write_to = open_out "resources/ifyouchangethisfileyouwillgetazero.txt" in
  write_top_counter write_to score_list 5;
  close_out write_to; ()
