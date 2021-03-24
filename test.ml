open OUnit2
open Board

(** Testing helper functions! *)
let cost_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_square board name) ~printer:string_of_int

let mortgage_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (mortgage_of_square board name) ~printer:string_of_int

let position_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (position_of_square board name) ~printer:string_of_int

let type_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (type_of_square board name) ~printer:String.escaped

let upgrade_cost_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (upgrade_cost board name) ~printer:string_of_int

let cost_of_tier_0_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_0_rent board name) ~printer:string_of_int

let cost_of_tier_1_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_1_rent board name) ~printer:string_of_int

let cost_of_tier_2_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_2_rent board name) ~printer:string_of_int

let cost_of_tier_3_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_3_rent board name) ~printer:string_of_int

let cost_of_tier_4_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_4_rent board name) ~printer:string_of_int

let cost_of_tier_5_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_5_rent board name) ~printer:string_of_int

(********************************************************************
   End helper functions.
 ********************************************************************)

let basic_board = from_json (Yojson.Basic.from_file "basic_board.json")
let board_test =
  [ 
    
  ]

let suite =
  "test suite for Final project"
  >::: List.flatten [ board_test; ]

let _ = run_test_tt_main suite
