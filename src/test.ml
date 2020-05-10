open OUnit2
open Audio
open State


(* 
Test plan: 

In this testing suite, we approached testing much like how testing was 
approached for A3. This test suite only tests the state.ml module because the
state module is the core of the game, and any sort of graphics, audio, menu, or 
game testing would be better suited for play testing as opposed to ounit tests. 
The functions we tested in our ounit suite were update, move, rotate, hold, and 
hard_drop. We didn't test any other function as they were incidentally tested
by the other tests within our testing file, and the tetromino file was
tested as a byproduct of our ounit suite. Not all specific tetrominos were 
tested for every function. This is because the tetrominos coding allows one test
to essentially cover the same function for all of the tetrominos. Along with
this, we play tested extensively to catch any bugs related to tetromino pieces
themselves, and found none. 

Developing test cases for this project was done in a glass box style due to the
implementation of state being somewhat complex. In Tetris, there are too many
background parts of state that need to be kept track of. However, in order to 
preserve the black box testing ideals, all state changes visible to players were
designed as tests prior to coding, and the resulting hidden values were
calculated based on which values state keeps track of. In order to ensure the
integrity of these tests, we calculated the hidden values based on the 
detailed specifications of our fucntions and NOT the code itself. By doing this,
we were able to develop a set of comprehensive tests that account for both the 
seen and hidden values, writing tests to fit expected outcomes, and not tests to
fit specific code.

This testing approach demonstrates the correctness of the system in three major
ways. First of all, the state tests ensure that any specific game movement is 
correct, and that any call of those functions in a game loop would execute 
properly. Second, play testing allowed us to make sure that the game loop ran
without error. Much like with A3, any sort of errors while playing revealed
issues in the system that we could fix, and it also ensured that our audio and 
visual modules were running as expected. When we were play testing, we made sure
to play in normal, and unusual ways in order to expose errors across the board 
with gameplay. It was also a lot easier to play test with the knowledge that the
movements in game were correct. It ensured that we didn't have to test specific 
game movements, and could focus on big picture bugs. Finally, we had others play 
test the system and record errors so that we had a more developed set of play 
tests, ensuring that we caught all errors prior to launch. In this way, our
testing approached ensured that we had a correct system.
*)


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
        arr.(i).(j) <- Some (0, 255, 255)
      done
    done;
    make_test_array_help t arr

(** [make_test_array ranges] is the array filled with arbitrary color values
    in the rows with ranges first tuple, and columns being the second tuple of 
    values for the whole list.  *)
let make_test_array (ranges : ((int*int)*(int*int)) list) = 
  let arr = Array.make_matrix 40 10 None in
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


let test_state_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_1_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [Rotate] []
    None false tet_t 1 (4, 20) 37 (Array.make_matrix 40 10 None)

let test_state_1_2 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [Rotate] []
    None false tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_2 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (3, 20) 38 (Array.make_matrix 40 10 None)

let test_state_2_1_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 
    [Movement] [] None false tet_t 0 (3, 20) 38 (Array.make_matrix 40 10 None)

let test_state_2_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (2, 20) 38 (Array.make_matrix 40 10 None)

let test_state_2_1_2 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 
    [Movement] [] None false tet_t 0 (2, 20) 38 (Array.make_matrix 40 10 None)

let test_state_2_2 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [Movement] 
    [] None false tet_t 0 (3, 20) 38 (Array.make_matrix 40 10 None)

let test_state_2_3 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 
    [Movement; Movement] [] None false tet_t 0 (4, 20) 38 
    (Array.make_matrix 40 10 None)

let test_state_3 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 1 (4, 20) 37 (Array.make_matrix 40 10 None)

let test_state_3_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 2 (4, 20) 37 (Array.make_matrix 40 10 None)

let test_state_3_1_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [Rotate] 
    [] None false tet_t 2 (4, 20) 37 (Array.make_matrix 40 10 None)

let test_state_3_2 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 3 (4, 20) 37 (Array.make_matrix 40 10 None)

let test_state_3_2_1 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [Rotate] []
    None false tet_t 3 (4, 20) 37 (Array.make_matrix 40 10 None)

let test_state_4 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (0, 20) 38 (Array.make_matrix 40 10 None)

let test_state_5 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (7, 20) 38 (Array.make_matrix 40 10 None)

let test_state_6 : TestS.t = State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (tet_i) 1 (7, 33)  36
    (make_test_array [((10,39),(0,8))])

let test_state_7_1 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_7_2 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [Rotate] 
    [] None false tet_t 1 (3, 37) 37 (Array.make_matrix 40 10 None)

let test_state_7_3 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [Rotate] 
    [] None false tet_t 3 (5, 37) 37 (Array.make_matrix 40 10 None)

let test_state_8_1 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false (tet_i) 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_8_2 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [Rotate] 
    [] None false (tet_i) 1 (5, 36) 36 (Array.make_matrix 40 10 None)

let test_state_8_3 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [Rotate] 
    [] None false (tet_i) 3 (3, 36) 36 (Array.make_matrix 40 10 None)

let test_state_9 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 0 [] [] None 
    false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_9_1 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 0 (3, 38) 38 (Array.make_matrix 40 10 None)

let test_state_9_1_1 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 
    [Movement] [] None false tet_t 0 (3, 38) 38 (Array.make_matrix 40 10 None)

let test_state_9_2 : TestS.t =  State.make_test_state 0 0 1 0 0 2 0 0 [] [] None 
    false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_9_2_1 : TestS.t =  State.make_test_state 0 0 1 0 0 2 0 0 
    [Movement] [] None false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_9_3 : TestS.t =  State.make_test_state 0 0 1 0 0 1 480 0 [] [] 
    None false tet_t 0 (3, 38) 38 (Array.make_matrix 40 10 None)

let test_state_10 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 0 (4, 36) 38 (Array.make_matrix 40 10 None)

let test_state_10_1 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 
    [Movement] [] None false tet_t 0 (3, 36) 38 (Array.make_matrix 40 10 None)

let test_state_10_1_1 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 
    [Movement; Movement] [] None false tet_t 0 (4, 36) 38 
    (Array.make_matrix 40 10 None)

let test_state_10_2 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 [] [] None 
    false tet_t 1 (4, 36) 37 (Array.make_matrix 40 10 None)

let test_state_10_2_1 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 
    [Rotate] [] None false tet_t 1 (4, 36) 37 (Array.make_matrix 40 10 None)

let test_state_10_2_2 : TestS.t =  State.make_test_state 0 0 1 0 0 1 0 0 
    [Rotate] [] None false tet_t 0 (4, 36) 38 (Array.make_matrix 40 10 None)

let test_state_11 : TestS.t =  State.make_test_state 0 0 1 0 0 0 60 0 [] [] None 
    false tet_t 0 (0, 38) 38 (Array.make_matrix 40 10 None)

let test_state_11_1 : TestS.t =  State.make_test_state 0 0 1 0 0 0 60 0 [] [] 
    None false tet_t 0 (7, 38) 38 (Array.make_matrix 40 10 None)

let movement_tests = [
  make_move_test "Move Left" test_state_1 `Left test_state_2_1_1;
  make_move_test "Move Left Again" test_state_2 `Left test_state_2_1_2;
  make_move_test "Move Right" test_state_2_1 `Right test_state_2_2;
  make_move_test "Move Right Again" test_state_2_2 `Right test_state_2_3;
  make_move_test "Move Left fail" test_state_4 `Left test_state_4;
  make_move_test "Move Right fail" test_state_5 `Right test_state_5;
  make_rotate_test "Rotate Clock Wise - 90" test_state_1 `CW test_state_1_1;
  make_rotate_test "Rotate Clock Wise - 180" test_state_3 `CW test_state_3_1_1;
  make_rotate_test "Rotate Clock Wise - 270" test_state_3_1 `CW 
    test_state_3_2_1;
  make_rotate_test "Rotate Clock Wise - 0" test_state_3_2 `CW test_state_1_2;
  make_rotate_test "Rotate Counter Clock Wise - 270" test_state_1 `CCW 
    test_state_3_2_1;
  make_rotate_test "Rotate Counter Clock Wise - 180" test_state_3_2 `CCW 
    test_state_3_1_1;
  make_rotate_test "Rotate Counter Clock Wise - 90" test_state_3_1 `CCW 
    test_state_1_1;
  make_rotate_test "Rotate Counter Clock Wise - 0" test_state_3 `CCW 
    test_state_1_2;
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
    test_state_9_1_1;
  make_move_test "Extended Movement Right Move" test_state_9_1 `Right 
    test_state_9_2_1;
  make_move_test "Extended Movement Timer reset" test_state_9_3 `Right 
    test_state_9_2_1;
  make_move_test "Extended Move Left Count test" test_state_10 `Left 
    test_state_10_1;
  make_move_test "Extended Move Right Count test" test_state_10_1 `Right
    test_state_10_1_1;
  make_rotate_test "Extended Rotate clockwise Count test" test_state_10 `CW
    test_state_10_2_1;
  make_rotate_test "Extended Rotate counter clockwise Count test" 
    test_state_10_2 `CCW test_state_10_2_2;
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

let make_gameover_test
    (name : string)
    (state : TestS.t)
    (delta : int)
    (soft_drop : bool)
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> 
      assert_raises (State.Local.Gameover(expected_output)) 
        (fun _ -> TestS.update state delta soft_drop))

let test_state_1 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 20 [] [] None 
    false tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_1_1 : TestS.t =  State.make_test_state 0 0 1 500 60 0 0 20 [] [] 
    None false tet_t 0 (4, 20) 38 
    (Array.make_matrix 40 10 None)

let test_state_1_2 : TestS.t =  State.make_test_state 0 0 1 500 120 0 0 20 [] [] 
    None false tet_t 0 (4, 20) 38 
    (Array.make_matrix 40 10 None)

let test_state_1_3 : TestS.t =  State.make_test_state 0 0 1 500 50 0 0 20 [] [] 
    None false tet_t 0 (4, 20) 38 
    (Array.make_matrix 40 10 None)

let test_state_2 : TestS.t =  State.make_test_state 0 0 1 500 480 0 0 20 [] [] 
    None false tet_t 0 (4, 20) 38 
    (Array.make_matrix 40 10 None)

let test_state_2_1 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 21 [] [] 
    None false tet_t 0 (4, 21) 38 
    (Array.make_matrix 40 10 None)

let test_state_2_2 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 22 [] [] 
    None false tet_t 0 (4, 22) 38 
    (Array.make_matrix 40 10 None)

let test_state_3 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 20 [] [] 
    None false tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_3_1 : TestS.t =  State.make_test_state 0 0 1 500 100 0 0 20 [] [] 
    None false tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_3_2 : TestS.t =  State.make_test_state 1 0 1 500 0 0 0 21 [] [] 
    None false tet_t 0 (4, 21) 38 
    (Array.make_matrix 40 10 None)

let test_state_4 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 38 [] [] 
    None false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_4_1 : TestS.t =  State.make_test_state 0 0 1 500 480 0 0 38 [] [] 
    None false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_4_2 : TestS.t =  State.make_test_state 0 0 1 500 60 0 0 38 [] [] 
    None false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_4_3 : TestS.t =  State.make_test_state 0 0 1 500 540 0 60 38 [] 
    [] None false tet_t 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_5 : TestS.t =  State.make_test_state 0 0 1 500 480 0 480 38 [] 
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_i 0 (4, 38) 38 (Array.make_matrix 40 10 None)

let test_state_5_1 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 20 
    [Locking] [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 37 (make_test_array [((39,39),(4,7))])

let test_state_6 : TestS.t =  State.make_test_state 0 0 1 500 480 0 480 36 []
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None false 
    tet_i 3 (8, 36) 36 (make_test_array [((39,39),(0,8))])

let test_state_6_1 : TestS.t =  State.make_test_state 100 1 1 1000 0 0 0 20 
    [LineClear; Locking] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 38 (make_test_array [((37,39),(9,9))])

let test_state_6_2 : TestS.t =  State.make_test_state 0 0 1 500 480 0 480 36 
    [] [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] 
    None false tet_i 3 (8, 36) 36 (make_test_array [((38,39),(0,8))])

let test_state_6_3 : TestS.t =  State.make_test_state 300 2 1 1000 0 0 0 20 
    [LineClear; Locking] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 38 (make_test_array [((38,39),(9,9))])

let test_state_6_4 : TestS.t =  State.make_test_state 0 0 1 500 480 0 480 36 
    [] [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] 
    None false tet_i 3 (8, 36) 36 (make_test_array [((37,39),(0,8))])

let test_state_6_5 : TestS.t =  State.make_test_state 500 3 1 1000 0 0 0 20 
    [LineClear; Locking] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 38 (make_test_array [((39,39),(9,9))])

let test_state_6_6 : TestS.t =  State.make_test_state 0 0 1 500 480 0 480 36 []
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None false 
    tet_i 3 (8, 36) 36 (make_test_array [((36,39),(0,8))])

let test_state_6_7 : TestS.t =  State.make_test_state 800 4 1 1000 0 0 0 20 
    [LineClear; Locking] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 38 (make_test_array [])

let test_state_6_8 : TestS.t =  State.make_test_state 0 9 1 500 480 0 480 36 []
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None false 
    tet_i 3 (8, 36) 36 (make_test_array [((39,39),(0,8))])

let test_state_6_9 : TestS.t =  State.make_test_state 200 10 2 793 0 0 0 20 
    [LineClear; Locking] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 38 (make_test_array [((37,39),(9,9))])

let test_state_7 : TestS.t =  State.make_test_state 0 0 1 1000 960 0 480 19 [] 
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_i 0 (5, 19) 19 (make_test_array [((21,39),(0,8))])

let test_state_7_1 : TestS.t =  State.make_test_state 0 0 1 1000 1020 0 480 19 
    [EndGame; Locking] [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_j 0 (5, 19) 19 
    (make_test_array [((21,39),(0,8)); ((20,20),(5,8))])



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
  make_update_test "Place piece" test_state_5 60 false test_state_5_1;
  make_update_test "Clear one line" test_state_6 60 false test_state_6_1;
  make_update_test "Clear two lines" test_state_6_2 60 false test_state_6_3;
  make_update_test "Clear three lines" test_state_6_4 60 false test_state_6_5;
  make_update_test "Tetris!" test_state_6_6 60 false test_state_6_7;
  make_update_test "Level up" test_state_6_8 60 false test_state_6_9;
  make_gameover_test "Gameover" test_state_7 60 false test_state_7_1
]


let make_hold_test
    (name : string)
    (state : TestS.t)
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (TestS.hold state))

let test_state_1 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 20 [] 
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_1_1 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 20 [] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] 
    (Some tet_t) true tet_j 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_1_2 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 20 [] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] 
    (Some tet_t) false tet_j 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_1_3 : TestS.t =  State.make_test_state 0 0 1 0 0 0 0 20 [] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] 
    (Some tet_j) true tet_t 0 (4, 20) 38 (Array.make_matrix 40 10 None)


let hold_tests = [
  make_hold_test "Hold piece when None held" test_state_1 test_state_1_1;
  make_hold_test "Hold piece when just held None" test_state_1_1 test_state_1_1;
  make_hold_test "Hold piece when Some held" test_state_1_2 test_state_1_3;
  make_hold_test "Hold piece when just held Some" test_state_1_3 test_state_1_3
]


let make_hard_drop_test
    (name : string)
    (state : TestS.t)
    (expected_output : TestS.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (TestS.hard_drop state))


let test_state_1 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 20 [] 
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_i 0 (4, 20) 38 (Array.make_matrix 40 10 None)

let test_state_1_1 : TestS.t =  State.make_test_state 36 0 1 500 0 0 0 20 
    [Locking] [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 37 (make_test_array [((39,39),(4,7))])

let test_state_1_2 : TestS.t =  State.make_test_state 0 0 1 500 0 0 0 20 [] 
    [tet_j; tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i]  
    None false tet_i 0 (4, 20) 38 (
    make_test_array [((39,39),(0,3));((39,39),(8,9))])

let test_state_1_3 : TestS.t =  State.make_test_state 136 1 1 1000 0 0 0 20 
    [LineClear; Locking] 
    [tet_o; tet_i; tet_l; tet_s; tet_t; tet_z; tet_o; tet_i] None 
    false tet_j 0 (4, 20) 38 (make_test_array [])



let hard_drop_tests = [
  make_hard_drop_test "Hard drop" test_state_1 test_state_1_1;
  make_hard_drop_test "Hard drop line clear" test_state_1_2 test_state_1_3
]

let tests = 
  "test suite for Tetris" >::: List.flatten [
    movement_tests;
    update_tests;
    hold_tests;
    hard_drop_tests
  ]

let _ = run_test_tt_main tests