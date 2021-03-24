open OUnit2
open Board

(** Testing helper functions! *)
let cost_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_square board name) ~printer:string_of_int

let cost_of_square_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_square board name)

let mortgage_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (mortgage_of_square board name) ~printer:string_of_int

let mortgage_of_square_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> mortgage_of_square_test board name)

let position_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (position_of_square board name) ~printer:string_of_int

let position_of_square_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> position_of_square board name)

let type_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (type_of_square board name) ~printer:String.escaped

let type_of_square_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> type_of_square board name)

let contains_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (contains board name) ~printer:string_of_bool

let upgrade_cost_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (upgrade_cost board name) ~printer:string_of_int

let upgrade_cost_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> upgrade_cost board name)

let cost_of_tier_0_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_0_rent board name) ~printer:string_of_int

let cost_of_tier_0_rent_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_tier_0_rent board name)

let cost_of_tier_1_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_1_rent board name) ~printer:string_of_int

let cost_of_tier_1_rent_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_tier_1_rent board name)

let cost_of_tier_2_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_2_rent board name) ~printer:string_of_int

let cost_of_tier_2_rent_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_tier_2_rent board name)

let cost_of_tier_3_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_3_rent board name) ~printer:string_of_int

let cost_of_tier_3_rent_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_tier_3_rent board name)

let cost_of_tier_4_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_4_rent board name) ~printer:string_of_int

let cost_of_tier_4_rent_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_tier_4_rent board name)

let cost_of_tier_5_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output 
  (cost_of_tier_5_rent board name) ~printer:string_of_int

let cost_of_tier_5_rent_error test_name board name excption =
  test_name >:: fun _ -> 
  assert_raises (excption) 
  (fun () -> cost_of_tier_5_rent board name)

(********************************************************************
   End helper functions.
********************************************************************)

let basic_board = from_json (Yojson.Basic.from_file "basic_board.json")
let faulty_board = from_json (Yojson.Basic.from_file "faulty_board.json")

let contains_compilation = 
  [
    contains_test "valid square" basic_board "Pennsylvania Railroad" true;
    contains_test "invalid square" basic_board "pennsylvania railroad" false; 
  ]
  
let cost_of_square_compilation =
  [
    cost_of_square_test "valid railroad" basic_board "Pennsylvania Railroad" 200;
    cost_of_square_error "invalid railroad" basic_board "pennsylvania railroad" 
    (UnknownSquare "pennsylvania railroad");
    cost_of_square_error "valid railroad, mismatch type" faulty_board 
    "Pennsylvania Railroad" (TypeMismatch "Not Railroad");
    cost_of_square_error "valid railroad, no cost" faulty_board "
    Reading Railroad - No Cost" (InvalidSquare ("Railroad", "Reading Railroad - No Cost"));

    cost_of_square_test "valid street" basic_board "Mediterranean Avenue" 60;
    cost_of_square_error "invalid street" basic_board "mediterranean avenue" 
    (UnknownSquare "mediterranean avenue");
    cost_of_square_error "valid street, mismatch type" faulty_board 
    "Mediterranean Avenue" (TypeMismatch "Streets");
    cost_of_square_error "valid street, no cost" faulty_board "
    Baltic Avenue - No Cost" (InvalidSquare ("Street", "Baltic Avenue - No Cost"));

    cost_of_square_test "income tax" basic_board "Income Tax" 200;
    cost_of_square_error "invalid income tax" basic_board "income tax" 
    (UnknownSquare "income tax");
    cost_of_square_error "valid income tax, mismatch type" faulty_board 
    "Income Tax" (TypeMismatch "Income tax");
    cost_of_square_error "valid income tax, no cost" faulty_board "
    Income Tax Bad" (InvalidSquare ("Income Tax", "Income Tax Bad"));

    cost_of_square_test "luxury tax" basic_board "Luxury Tax" 75;
    cost_of_square_error "invalid luxury tax" basic_board "luxury tax" 
    (UnknownSquare "luxury tax");
    cost_of_square_error "valid luxury tax, mismatch type" faulty_board 
    "Luxury Tax" (TypeMismatch "Luxury tax");
    cost_of_square_error "valid luxury tax, no cost" faulty_board "
    Luxury Tax - No Cost" (InvalidSquare ("Luxury Tax", "Luxury Tax - No Cost"));

    cost_of_square_test "water works" basic_board "Water Works" 250;
    cost_of_square_error "invalid water works" basic_board "water works" 
    (UnknownSquare "water works");
    cost_of_square_error "valid water works, mismatch type" faulty_board 
    "Water Works" (TypeMismatch "utility");
    cost_of_square_error "valid water works, no cost" faulty_board "
    Water Works - No Cost" (InvalidSquare ("Water Works", "Water Works - No Cost"));

    cost_of_square_error "invalid type - go" basic_board "Go" 
    (TypeMismatch "Go");
    cost_of_square_error "invalid type - jail/just visiting" basic_board 
    "Jail/Just Visiting" (TypeMismatch "Jail/Just Visiting");
    cost_of_square_error "invalid type - chance" basic_board 
    "Chance" (TypeMismatch "Chance");
    cost_of_square_error "invalid type - community chest" basic_board 
    "Community Chest" (TypeMismatch "Community Chest");
    cost_of_square_error "invalid type - free parking" basic_board 
    "Free Parking" (TypeMismatch "Free Parking");
    cost_of_square_error "invalid type - go to jail" basic_board 
    "Go to Jail" (TypeMismatch "Go to Jail");
  ]


let suite =
  "test suite for Final project"
  >::: List.flatten [ contains_compilation; cost_of_square_compilation; ]

let _ = run_test_tt_main suite
