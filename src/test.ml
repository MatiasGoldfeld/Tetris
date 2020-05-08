open OUnit2
open Audio
open State


let test =
  ()
(* Sdl.init [`AUDIO];
   let audio = init "./resources/" in
   start_music audio;
   Sdltimer.delay(1000); *)

module TestS = Local



let rec make_test_array_help lst arr =
  match lst with
  | [] -> arr
  | h::t -> 
    let rows = fst h in
    let columns = snd h in
    for i = fst rows to snd rows do
      for j = fst columns to snd columns do
        arr.(i).(j) <- Some (212, 175, 55)
      done
    done;
    make_test_array_help t arr

(** [make_test_array ranges] is the array filled with arbitrary color values
    in the rows with ranges first tuple, and columns being the second tuple of 
    values for the whole list.  *)
let make_test_array (ranges : ((int*int)*(int*int)) list) = 
  let arr = Array.make_matrix 20 10 None in
  make_test_array_help ranges arr


let make_move_test
    (name : string)
    (state : TestS.t)
    (direction : [`Left | `Right])
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> assert_equal expected_output (TestS.move direction state))


let make_rotate_test
    (name : string)
    (state : TestS.t)
    (rotation : [`CCW | `CW])
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> assert_equal expected_output (TestS.rotate rotation state))


let test_state_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "t") 0 (4, 0) 18 (Array.make_matrix 20 10 None)

let test_state_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "t") 0 (3, 0) 18 (Array.make_matrix 20 10 None)

let test_state_3 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "t") 1 (4, 0) 17 (Array.make_matrix 20 10 None)

let test_state_4 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "t") 0 (0, 0) 18 (Array.make_matrix 20 10 None)

let test_state_5 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "t") 0 (7, 0) 18 (Array.make_matrix 20 10 None)

let test_state_6 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "i") 1 (7, 13)  16
    (make_test_array [((10,19),(0,8))])

let test_state_71 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (Tetromino.get_tet "t") 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_72 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false (Tetromino.get_tet "t") 1 (3, 17) 17 (Array.make_matrix 20 10 None)




let movement_tests = [
  make_move_test "Move Left" test_state_1 `Left test_state_2;
  make_move_test "Move Right" test_state_2 `Right test_state_1;
  make_move_test "Move Left fail" test_state_4 `Left test_state_4;
  make_move_test "Move Right fail" test_state_5 `Right test_state_5;
  make_rotate_test "Rotate Clock Wise" test_state_1 `CW test_state_3;
  make_rotate_test "Rotate Counter Clock Wise" test_state_3 `CCW test_state_1;
  make_rotate_test "Rotate Clock Wise fail" test_state_6 `CW test_state_6;
  make_rotate_test "Rotate Counter Clock Wise fail" test_state_6 `CCW 
    test_state_6;
  make_rotate_test "Floor kick 3x3 test" test_state_71 `CW test_state_72
]










let tests = 
  "test suite for Tetris" >::: List.flatten [
    movement_tests
  ]



let _ = run_test_tt_main tests