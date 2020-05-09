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

let tet_t = Tetromino.get_tet "t"
let tet_i = Tetromino.get_tet "i"
let tet_l = Tetromino.get_tet "l"
let tet_j = Tetromino.get_tet "j"
let tet_s = Tetromino.get_tet "s"
let tet_z = Tetromino.get_tet "z"
let tet_o = Tetromino.get_tet "o"

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
    false tet_t 0 (4, 0) 18 (Array.make_matrix 20 10 None)

let test_state_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (3, 0) 18 (Array.make_matrix 20 10 None)

let test_state_2_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (2, 0) 18 (Array.make_matrix 20 10 None)

let test_state_3 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 1 (4, 0) 17 (Array.make_matrix 20 10 None)

let test_state_3_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 2 (4, 0) 17 (Array.make_matrix 20 10 None)

let test_state_3_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 3 (4, 0) 17 (Array.make_matrix 20 10 None)

let test_state_4 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (0, 0) 18 (Array.make_matrix 20 10 None)

let test_state_5 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (7, 0) 18 (Array.make_matrix 20 10 None)

let test_state_6 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (tet_i) 1 (7, 13)  16
    (make_test_array [((10,19),(0,8))])

let test_state_7_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_7_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 1 (3, 17) 17 (Array.make_matrix 20 10 None)

let test_state_7_3 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 3 (5, 17) 17 (Array.make_matrix 20 10 None)

let test_state_8_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (tet_i) 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_8_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false (tet_i) 1 (5, 16) 16 (Array.make_matrix 20 10 None)

let test_state_8_3 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false (tet_i) 3 (3, 16) 16 (Array.make_matrix 20 10 None)

let test_state_9 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_9_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 0 (3, 18) 18 (Array.make_matrix 20 10 None)

let test_state_9_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 2 0 0 [] [] None 
    false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_9_3 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 480 0 [] [] 
    None false tet_t 0 (3, 18) 18 (Array.make_matrix 20 10 None)

let test_state_10 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 0 (4, 16) 18 (Array.make_matrix 20 10 None)

let test_state_10_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 0 (3, 16) 18 (Array.make_matrix 20 10 None)

let test_state_10_2 : TestS.t = TestS.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 1 (4, 16) 17 (Array.make_matrix 20 10 None)

let test_state_11 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 60 0 [] [] None 
    false tet_t 0 (0, 18) 18 (Array.make_matrix 20 10 None)

let test_state_11_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 60 0 [] [] None 
    false tet_t 0 (7, 18) 18 (Array.make_matrix 20 10 None)

let movement_tests = [
  make_move_test "Move Left" test_state_1 `Left test_state_2;
  make_move_test "Move Left Again" test_state_2 `Left test_state_2_1;
  make_move_test "Move Right" test_state_2_1 `Right test_state_2;
  make_move_test "Move Right Again" test_state_2 `Right test_state_1;
  make_move_test "Move Left fail" test_state_4 `Left test_state_4;
  make_move_test "Move Right fail" test_state_5 `Right test_state_5;
  make_rotate_test "Rotate Clock Wise - 90" test_state_1 `CW test_state_3;
  make_rotate_test "Rotate Clock Wise - 180" test_state_3 `CW test_state_3_1;
  make_rotate_test "Rotate Clock Wise - 270" test_state_3_1 `CW test_state_3_2;
  make_rotate_test "Rotate Clock Wise - 0" test_state_3_2 `CW test_state_1;
  make_rotate_test "Rotate Counter Clock Wise - 270" test_state_1 `CCW 
    test_state_3_2;
  make_rotate_test "Rotate Counter Clock Wise - 180" test_state_3_2 `CCW 
    test_state_3_1;
  make_rotate_test "Rotate Counter Clock Wise - 90" test_state_3_1 `CCW 
    test_state_3;
  make_rotate_test "Rotate Counter Clock Wise - 0" test_state_3 `CCW test_state_1;
  make_rotate_test "Rotate Clock Wise fail" test_state_6 `CW test_state_6;
  make_rotate_test "Rotate Counter Clock Wise fail" test_state_6 `CCW 
    test_state_6;
  make_rotate_test "Floor kick 3x3 test clockwise" test_state_7_1 `CW 
    test_state_7_2;
  make_rotate_test "Floor kick 3x3 test counter clockwise" test_state_7_1 `CCW 
    test_state_7_3;
  make_rotate_test "Floor kick 4x4 test clockwise" test_state_8_1 `CW 
    test_state_8_2;
  make_rotate_test "Floor kick 4x4 test clockwise" test_state_8_1 `CCW 
    test_state_8_3;
  make_move_test "Extended Movement Left Move" test_state_9 `Left 
    test_state_9_1;
  make_move_test "Extended Movement Right Move" test_state_9_1 `Right 
    test_state_9_2;
  make_move_test "Extended Movement Timer reset" test_state_9_3 `Right 
    test_state_9_2;
  make_move_test "Extended Move Left Count test" test_state_10 `Left 
    test_state_10_1;
  make_move_test "Extended Move Right Count test" test_state_10_1 `Right
    test_state_10;
  make_rotate_test "Extended Rotate clockwise Count test" test_state_10 `CW
    test_state_10_2;
  make_rotate_test "Extended Rotate counter clockwise Count test" 
    test_state_10_2 `CCW test_state_10;
  make_move_test "Extended placement delta Left fail" test_state_11 `Left 
    test_state_11;
  make_move_test "Extended placement delta Left fail" test_state_11_1 `Right 
    test_state_11_1
] 


let make_update_test
    (name : string)
    (state : TestS.t)
    (delta : int)
    (soft_drop : bool)
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (TestS.update state delta soft_drop))

let test_state_1 : TestS.t = TestS.make_test_state 0 0 1 500 0 0 0 0 [] [] None 
    false tet_t 0 (4, 0) 18 (Array.make_matrix 20 10 None)

let test_state_1_1 : TestS.t = TestS.make_test_state 0 0 1 500 60 0 0 0 [] [] 
    None false tet_t 0 (4, 0) 18 
    (Array.make_matrix 20 10 None)

let test_state_1_2 : TestS.t = TestS.make_test_state 0 0 1 500 120 0 0 0 [] [] 
    None false tet_t 0 (4, 0) 18 
    (Array.make_matrix 20 10 None)

let test_state_1_3 : TestS.t = TestS.make_test_state 0 0 1 500 50 0 0 0 [] [] 
    None false tet_t 0 (4, 0) 18 
    (Array.make_matrix 20 10 None)

let test_state_2 : TestS.t = TestS.make_test_state 0 0 1 500 480 0 0 0 [] [] 
    None false tet_t 0 (4, 0) 18 
    (Array.make_matrix 20 10 None)

let test_state_2_1 : TestS.t = TestS.make_test_state 0 0 1 500 0 0 0 1 [] [] 
    None false tet_t 0 (4, 1) 18 
    (Array.make_matrix 20 10 None)

let test_state_2_2 : TestS.t = TestS.make_test_state 0 0 1 500 0 0 0 2 [] [] 
    None false tet_t 0 (4, 2) 18 
    (Array.make_matrix 20 10 None)

let test_state_3 : TestS.t = TestS.make_test_state 0 0 1 500 0 0 0 0 [] [] 
    None false tet_t 0 (4, 0) 18 (Array.make_matrix 20 10 None)

let test_state_3_1 : TestS.t = TestS.make_test_state 0 0 1 500 100 0 0 0 [] [] 
    None false tet_t 0 (4, 0) 18 (Array.make_matrix 20 10 None)

let test_state_3_2 : TestS.t = TestS.make_test_state 1 0 1 500 0 0 0 1 [] [] 
    None false tet_t 0 (4, 1) 18 
    (Array.make_matrix 20 10 None)

let test_state_4 : TestS.t = TestS.make_test_state 0 0 1 500 0 0 0 18 [] [] None 
    false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_4_1 : TestS.t = TestS.make_test_state 0 0 1 500 480 0 0 18 [] [] 
    None false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_4_2 : TestS.t = TestS.make_test_state 0 0 1 500 60 0 0 18 [] [] 
    None false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let test_state_4_3 : TestS.t = TestS.make_test_state 0 0 1 500 540 0 60 18 [] [] 
    None false tet_t 0 (4, 18) 18 (Array.make_matrix 20 10 None)

let update_tests = [
  make_update_test "Update test, delta change" test_state_1 60 false 
    test_state_1_1;
  make_update_test "Update test, delta change insurance" test_state_1_1 60 false 
    test_state_1_2;
  make_update_test "Update test, delta change, different delta" test_state_1 
    50 false test_state_1_3;
  make_update_test "Step test 1" test_state_2 60 false test_state_2_1;
  make_update_test "Step test 2" test_state_2_1 500 false test_state_2_2;
  make_update_test "Update test, soft_drop" test_state_3 5 true 
    test_state_3_1;
  make_update_test "Step test, soft_drop" test_state_3 60 true test_state_3_2;
  make_update_test "Update on bottom collision" test_state_4 60 false 
    test_state_4_2;
  make_update_test "Step on bottom collision" test_state_4_1 60 false 
    test_state_4_3;
]


let make_hold_test
    (name : string)
    (state : TestS.t)
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (TestS.hold state))

let test_state_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] 
    [tet_o; tet_j; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_t 0 (4, 0) 18 (Array.make_matrix 20 10 None)

let test_state_1_1 : TestS.t = TestS.make_test_state 0 0 1 0 0 0 0 0 [] 
    [tet_j; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] 
    (Some tet_t) false tet_o 0 (4, 0) 18 (Array.make_matrix 20 10 None)


let hold_tests = [
  make_hold_test "Hold piece when none held" test_state_1 test_state_1_1




]



let tests = 
  "test suite for Tetris" >::: List.flatten [
    movement_tests;
    update_tests;
    (* hold_tests *)
  ]



let _ = run_test_tt_main tests