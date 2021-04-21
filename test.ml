open OUnit2
open Board
open Player

(** Testing helper functions! *)
let int_list_printer lst =
  let prettify_list = function
    | "" -> "[]"
    | str -> "[" ^ String.sub str 0 (String.length str - 2) ^ "]"
  in
  let rec print_list = function
    | [] -> ""
    | h :: t -> string_of_int h ^ "; " ^ print_list t
  in
  prettify_list (print_list lst)

(**[pp_properties lst] pretty-prints a list of properties. Not
   tail-recursive *)
let rec pp_properties = function
  | [] -> ""
  | x :: xs -> x ^ ", " ^ pp_properties xs

(**[pp_player p] pretty-prints player [p]*)
let pp_player p =
  "Name: " ^ name p ^ "\nPiece: " ^ piece p ^ "\nCurrent Square: "
  ^ string_of_int (current_square p) ^ "\nBalance: "
  ^ string_of_int (balance p)
  ^ "\nProperties: "
  ^ pp_properties (properties p)
  ^ "\nIs bankrupt: "
  ^ string_of_bool (bankrupt p)
  ^ "\nGet out of jail cards: "
  ^ string_of_int (jail_cards p)

let cost_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_square board name)
    ~printer:string_of_int

let field_test
    (name : string)
    (player : Player.t)
    f
    (expected_output : 'a) =
  name >:: fun _ -> assert_equal expected_output (f player)

let balance_test (name : string) (value : int) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output value ~printer:string_of_int

let cost_of_square_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_square board name)

let mortgage_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (mortgage_of_square board name)
    ~printer:string_of_int

let mortgage_of_square_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> mortgage_of_square board name)

let position_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (position_of_square board name)
    ~printer:int_list_printer

let position_of_square_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> position_of_square board name)

let type_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (type_of_square board name)
    ~printer:String.escaped

let type_of_square_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> type_of_square board name)

let nth_square_name_test test_name board pos expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (nth_square_name board pos)
    ~printer:String.escaped

let nth_square_name_error test_name board pos excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> nth_square_name board pos)

let next_twelve_test test_name board pos expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (next_twelve board pos)
    ~printer:String.escaped

let next_twelve_error test_name board pos excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> next_twelve board pos)

let contains_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output (contains board name)
    ~printer:string_of_bool

let upgrade_cost_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (upgrade_cost board name)
    ~printer:string_of_int

let upgrade_cost_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> upgrade_cost board name)

let set_of_square_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (set_of_square board name)
    ~printer:String.escaped

let set_of_square_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> set_of_square board name)

let cost_of_tier_0_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_tier_0_rent board name)
    ~printer:string_of_int

let cost_of_tier_0_rent_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_tier_0_rent board name)

let cost_of_tier_1_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_tier_1_rent board name)
    ~printer:string_of_int

let cost_of_tier_1_rent_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_tier_1_rent board name)

let cost_of_tier_2_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_tier_2_rent board name)
    ~printer:string_of_int

let cost_of_tier_2_rent_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_tier_2_rent board name)

let cost_of_tier_3_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_tier_3_rent board name)
    ~printer:string_of_int

let cost_of_tier_3_rent_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_tier_3_rent board name)

let cost_of_tier_4_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_tier_4_rent board name)
    ~printer:string_of_int

let cost_of_tier_4_rent_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_tier_4_rent board name)

let cost_of_tier_5_rent_test test_name board name expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output
    (cost_of_tier_5_rent board name)
    ~printer:string_of_int

let cost_of_tier_5_rent_error test_name board name excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> cost_of_tier_5_rent board name)

let get_chance_card_test test_name board expected_output =
  test_name >:: fun ctxt ->
  let card = fst (get_chance_card board) in
  assert_equal expected_output card.title ~printer:String.escaped

let get_chance_card_error test_name board excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> get_chance_card board)

let get_community_chest_card_test test_name board expected_output =
  test_name >:: fun ctxt ->
  let card = fst (get_community_chest_card board) in
  assert_equal expected_output card.title ~printer:String.escaped

let get_community_chest_card_error test_name board excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> get_community_chest_card board)

(********************************************************************
  End helper functions.
  ********************************************************************)

let basic_board = from_json (Yojson.Basic.from_file "basic_board.json")

let faulty_board =
  from_json (Yojson.Basic.from_file "faulty_board.json")

let contains_compilation =
  [
    contains_test "valid square" basic_board "Pennsylvania Railroad"
      true;
    contains_test "invalid square" basic_board "pennsylvania railroad"
      false;
  ]

let position_of_square_compilation =
  [
    position_of_square_test "valid square" basic_board "Go" [ 0 ];
    position_of_square_test "community chest" basic_board
      "Community Chest" [ 2; 17; 33 ];
    position_of_square_error "invalid square" basic_board "go"
      (UnknownSquare "go");
  ]

let cost_of_square_compilation =
  [
    cost_of_square_test "valid railroad" basic_board
      "Pennsylvania Railroad" 200;
    cost_of_square_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    cost_of_square_error "valid railroad, mismatch type" faulty_board
      "Pennsylvania Railroad" (UnknownType "Not Railroad");
    cost_of_square_error "valid railroad, no cost" faulty_board
      "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    cost_of_square_error "valid railroad, null cost" faulty_board
      "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    cost_of_square_test "valid street" basic_board
      "Mediterranean Avenue" 60;
    cost_of_square_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_square_error "valid street, no cost" faulty_board
      "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    cost_of_square_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_square_test "income tax" basic_board "Income Tax" 200;
    cost_of_square_error "valid income tax, mismatch type" faulty_board
      "Income Tax" (UnknownType "Income tax");
    cost_of_square_error "valid income tax, no cost" faulty_board
      "Income Tax Bad"
      (InvalidSquare ("Income Tax", "Income Tax Bad"));
    cost_of_square_error "valid income tax, null cost" faulty_board
      "Income Null"
      (InvalidSquare ("Income Tax", "Income Null"));
    cost_of_square_test "luxury tax" basic_board "Luxury Tax" 75;
    cost_of_square_error "valid luxury tax, mismatch type" faulty_board
      "Luxury Tax" (UnknownType "Luxury tax");
    cost_of_square_error "valid luxury tax, no cost" faulty_board
      "Luxury Tax - Bad"
      (InvalidSquare ("Luxury Tax", "Luxury Tax - Bad"));
    cost_of_square_error "valid luxury tax, null cost" faulty_board
      "Luxury Null"
      (InvalidSquare ("Luxury Tax", "Luxury Null"));
    cost_of_square_test "water works" basic_board "Water Works" 150;
    cost_of_square_error "valid water works, mismatch type" faulty_board
      "Water Works" (UnknownType "utility");
    cost_of_square_error "valid water works, no cost" faulty_board
      "Water Works - Bad"
      (InvalidSquare ("Utility", "Water Works - Bad"));
    cost_of_square_error "valid water works, null cost" faulty_board
      "Water Null"
      (InvalidSquare ("Utility", "Water Null"));
    cost_of_square_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_square_error "invalid type - jail/just visiting" basic_board
      "Jail/Just Visiting" (TypeMismatch "Jail/Just Visiting");
    cost_of_square_error "invalid type - chance" basic_board "Chance"
      (TypeMismatch "Chance");
    cost_of_square_error "invalid type - community chest" basic_board
      "Community Chest" (TypeMismatch "Community Chest");
    cost_of_square_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_square_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
  ]

let type_of_square_test_compilation =
  [
    type_of_square_test "valid square" basic_board "Boardwalk" "Street";
    type_of_square_error "invalid square" basic_board "go"
      (UnknownSquare "go");
    type_of_square_error "invalid square" faulty_board
      "Pennsylvania Railroad" (UnknownType "Not Railroad");
  ]

let nth_square_name_test_compilation =
  [
    nth_square_name_test "valid square" basic_board 0 "Go";
    nth_square_name_error "failure exp" basic_board 100 (Failure "nth");
    nth_square_name_error "invalid arg" basic_board (-10)
      (Invalid_argument "List.nth");
  ]

let next_twelve_test_compilation =
  [
    next_twelve_test "valid square - 0" basic_board 40 "| Go | Mediterranean Avenue | Community Chest | Baltic Avenue | Income Tax | Reading Railroad | Oriental Avenue | Chance | Vermont Avenue | Connecticut Avenue | Jail/Just Visiting | St. Charles Place | ";
    next_twelve_test "valid square - 39" basic_board 39 "| Boardwalk | Go | Mediterranean Avenue | Community Chest | Baltic Avenue | Income Tax | Reading Railroad | Oriental Avenue | Chance | Vermont Avenue | Connecticut Avenue | Jail/Just Visiting | ";
    next_twelve_error "invalid arg" basic_board (-10) (Invalid_argument "List.nth");
  ]

let mortgage_of_square_compilation =
  [
    mortgage_of_square_test "valid railroad" basic_board
      "Pennsylvania Railroad" 100;
    mortgage_of_square_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    mortgage_of_square_error "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    mortgage_of_square_error "valid railroad, no mortgage" faulty_board
      "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    mortgage_of_square_error "valid railroad, null mortgage"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    mortgage_of_square_test "valid street" basic_board
      "Mediterranean Avenue" 30;
    mortgage_of_square_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    mortgage_of_square_error "valid street, no mortgage" faulty_board
      "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    mortgage_of_square_error "valid street, null mortgage" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    mortgage_of_square_test "water works" basic_board "Water Works" 75;
    mortgage_of_square_error "valid water works, mismatch type"
      faulty_board "Water Works" (UnknownType "utility");
    mortgage_of_square_error "valid water works, no mortgage"
      faulty_board "Water Works - Bad"
      (InvalidSquare ("Utility", "Water Works - Bad"));
    mortgage_of_square_error "valid water works, null mortgage"
      faulty_board "Water Null"
      (InvalidSquare ("Utility", "Water Null"));
    mortgage_of_square_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    mortgage_of_square_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    mortgage_of_square_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    mortgage_of_square_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    mortgage_of_square_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    mortgage_of_square_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    mortgage_of_square_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    mortgage_of_square_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
  ]

let set_of_square_compilation =
  [
    set_of_square_test "valid railroad" basic_board
      "Pennsylvania Railroad" "Railroad";
    set_of_square_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    set_of_square_error "valid railroad, mismatch type" faulty_board
      "Pennsylvania Railroad" (UnknownType "Not Railroad");
    set_of_square_error "valid railroad, no set" faulty_board
      "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    set_of_square_error "valid railroad, null set" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    set_of_square_test "valid street" basic_board "Mediterranean Avenue"
      "brown";
    set_of_square_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    set_of_square_error "valid street, no set" faulty_board
      "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    set_of_square_error "valid street, null set" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    set_of_square_test "water works" basic_board "Water Works" "Utility";
    set_of_square_error "valid water works, mismatch type" faulty_board
      "Water Works" (UnknownType "utility");
    set_of_square_error "valid water works, no set" faulty_board
      "Water Works - Bad"
      (InvalidSquare ("Utility", "Water Works - Bad"));
    set_of_square_error "valid water works, null set" faulty_board
      "Water Null"
      (InvalidSquare ("Utility", "Water Null"));
    set_of_square_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    set_of_square_error "invalid type - jail/just visiting" basic_board
      "Jail/Just Visiting" (TypeMismatch "Jail/Just Visiting");
    set_of_square_error "invalid type - chance" basic_board "Chance"
      (TypeMismatch "Chance");
    set_of_square_error "invalid type - community chest" basic_board
      "Community Chest" (TypeMismatch "Community Chest");
    set_of_square_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    set_of_square_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    set_of_square_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    set_of_square_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
  ]

let upgrade_cost_compilation =
  [
    upgrade_cost_test "valid street" basic_board "Mediterranean Avenue"
      50;
    upgrade_cost_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    upgrade_cost_error "valid street, no upgrade cost" faulty_board
      "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    upgrade_cost_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    upgrade_cost_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    upgrade_cost_error "invalid type - jail/just visiting" basic_board
      "Jail/Just Visiting" (TypeMismatch "Jail/Just Visiting");
    upgrade_cost_error "invalid type - chance" basic_board "Chance"
      (TypeMismatch "Chance");
    upgrade_cost_error "invalid type - community chest" basic_board
      "Community Chest" (TypeMismatch "Community Chest");
    upgrade_cost_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    upgrade_cost_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    upgrade_cost_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    upgrade_cost_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    upgrade_cost_error "invalid type - railroad" basic_board
      "Reading Railroad" (TypeMismatch "Railroad");
    upgrade_cost_error "invalid type - go" basic_board "Water Works"
      (TypeMismatch "Utility");
  ]

let cost_of_tier_0_compilation =
  [
    cost_of_tier_0_rent_test "valid railroad" basic_board
      "Pennsylvania Railroad" 25;
    cost_of_tier_0_rent_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    cost_of_tier_0_rent_error "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    cost_of_tier_0_rent_error "valid railroad, no rent tiers period"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    cost_of_tier_0_rent_error "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    cost_of_tier_0_rent_error "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    cost_of_tier_0_rent_error "valid railroad, null tier" faulty_board
      "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    cost_of_tier_0_rent_error "valid railroad, null cost" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    cost_of_tier_0_rent_test "valid street" basic_board
      "Mediterranean Avenue" 2;
    cost_of_tier_0_rent_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_tier_0_rent_error "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    cost_of_tier_0_rent_error "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    cost_of_tier_0_rent_error "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    cost_of_tier_0_rent_error "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    cost_of_tier_0_rent_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_tier_0_rent_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_tier_0_rent_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    cost_of_tier_0_rent_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    cost_of_tier_0_rent_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    cost_of_tier_0_rent_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_tier_0_rent_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    cost_of_tier_0_rent_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    cost_of_tier_0_rent_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    cost_of_tier_0_rent_error "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_1_compilation =
  [
    cost_of_tier_1_rent_test "valid railroad" basic_board
      "Pennsylvania Railroad" 50;
    cost_of_tier_1_rent_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    cost_of_tier_1_rent_error "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    cost_of_tier_1_rent_error "valid railroad, no rent tiers period"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    cost_of_tier_1_rent_error "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    cost_of_tier_1_rent_error "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    cost_of_tier_1_rent_error "valid railroad, null tier" faulty_board
      "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    cost_of_tier_1_rent_error "valid railroad, null cost" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    cost_of_tier_1_rent_test "valid street" basic_board
      "Mediterranean Avenue" 10;
    cost_of_tier_1_rent_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_tier_1_rent_error "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    cost_of_tier_1_rent_error "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    cost_of_tier_1_rent_error "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    cost_of_tier_1_rent_error "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    cost_of_tier_1_rent_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_tier_1_rent_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_tier_1_rent_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    cost_of_tier_1_rent_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    cost_of_tier_1_rent_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    cost_of_tier_1_rent_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_tier_1_rent_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    cost_of_tier_1_rent_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    cost_of_tier_1_rent_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    cost_of_tier_1_rent_error "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_2_compilation =
  [
    cost_of_tier_2_rent_test "valid railroad" basic_board
      "Pennsylvania Railroad" 100;
    cost_of_tier_2_rent_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    cost_of_tier_2_rent_error "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    cost_of_tier_2_rent_error "valid railroad, no rent tiers period"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    cost_of_tier_2_rent_error "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    cost_of_tier_2_rent_error "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    cost_of_tier_2_rent_error "valid railroad, null tier" faulty_board
      "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    cost_of_tier_2_rent_error "valid railroad, null cost" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    cost_of_tier_2_rent_test "valid street" basic_board
      "Mediterranean Avenue" 30;
    cost_of_tier_2_rent_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_tier_2_rent_error "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    cost_of_tier_2_rent_error "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    cost_of_tier_2_rent_error "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    cost_of_tier_2_rent_error "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    cost_of_tier_2_rent_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_tier_2_rent_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_tier_2_rent_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    cost_of_tier_2_rent_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    cost_of_tier_2_rent_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    cost_of_tier_2_rent_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_tier_2_rent_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    cost_of_tier_2_rent_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    cost_of_tier_2_rent_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    cost_of_tier_2_rent_error "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_3_compilation =
  [
    cost_of_tier_3_rent_test "valid railroad" basic_board
      "Pennsylvania Railroad" 200;
    cost_of_tier_3_rent_error "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    cost_of_tier_3_rent_error "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    cost_of_tier_3_rent_error "valid railroad, no rent tiers period"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    cost_of_tier_3_rent_error "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    cost_of_tier_3_rent_error "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    cost_of_tier_3_rent_error "valid railroad, null tier" faulty_board
      "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    cost_of_tier_3_rent_error "valid railroad, null cost" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    cost_of_tier_3_rent_test "valid street" basic_board
      "Mediterranean Avenue" 90;
    cost_of_tier_3_rent_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_tier_3_rent_error "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    cost_of_tier_3_rent_error "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    cost_of_tier_3_rent_error "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    cost_of_tier_3_rent_error "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    cost_of_tier_3_rent_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_tier_3_rent_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_tier_3_rent_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    cost_of_tier_3_rent_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    cost_of_tier_3_rent_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    cost_of_tier_3_rent_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_tier_3_rent_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    cost_of_tier_3_rent_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    cost_of_tier_3_rent_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    cost_of_tier_3_rent_error "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_4_compilation =
  [
    cost_of_tier_4_rent_test "valid street" basic_board
      "Mediterranean Avenue" 160;
    cost_of_tier_4_rent_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_tier_4_rent_error "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    cost_of_tier_4_rent_error "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    cost_of_tier_4_rent_error "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    cost_of_tier_4_rent_error "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    cost_of_tier_4_rent_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_tier_4_rent_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_tier_4_rent_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    cost_of_tier_4_rent_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    cost_of_tier_4_rent_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    cost_of_tier_4_rent_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_tier_4_rent_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    cost_of_tier_4_rent_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    cost_of_tier_4_rent_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    cost_of_tier_4_rent_error "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
    cost_of_tier_4_rent_error "invalid type - railroad" basic_board
      "Reading Railroad" (TypeMismatch "Railroad");
  ]

let cost_of_tier_5_compilation =
  [
    cost_of_tier_5_rent_test "valid street" basic_board
      "Mediterranean Avenue" 250;
    cost_of_tier_5_rent_error "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    cost_of_tier_5_rent_error "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    cost_of_tier_5_rent_error "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    cost_of_tier_5_rent_error "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    cost_of_tier_5_rent_error "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    cost_of_tier_5_rent_error "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    cost_of_tier_5_rent_error "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    cost_of_tier_5_rent_error "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    cost_of_tier_5_rent_error "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    cost_of_tier_5_rent_error "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    cost_of_tier_5_rent_error "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    cost_of_tier_5_rent_error "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    cost_of_tier_5_rent_error "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    cost_of_tier_5_rent_error "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    cost_of_tier_5_rent_error "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
    cost_of_tier_5_rent_error "invalid type - railroad" basic_board
      "Reading Railroad" (TypeMismatch "Railroad");
  ]

let chance_community_compilation =
  [
    get_chance_card_test "valid chance" basic_board
      "Advance to Go (Collect $200)";
    get_chance_card_error "empty chance" faulty_board
      (Failure "No Chance Cards left");
    get_community_chest_card_test "valid chance" basic_board
      "Advance to Go (Collect $200)";
    get_community_chest_card_error "empty community" faulty_board
      (Failure "No Community Chests left");
  ]

let player1 : Player.t = create "Jacob" "Thimble"

let player2 = create "Constantine" "Top-hat"

(* let field_tests =
  [
    field_test "Player 1 Name" player1 name "Jacob";
    field_test "Player 2 Name" player2 name "Constantine";
    field_test "Player 1 Piece" player1 piece "Thimble";
    field_test "Player 2 Piece" player2 piece "Top-hat";
    field_test "Player 1 Not In Jail" player1 is_in_jail false;
    field_test "Player 2 Not in Jail" player2 is_in_jail false;
    field_test "Player 1 has no squares" player1 properties [];
    field_test "Player 2 has no squares" player2 properties [];
  ]

let p1_init = balance player1

let p2_init = balance player2

let x = bank_transaction 300 player1

let x = balance player1

let y = bank_transaction (-500) player2

let y = balance player2

let p1_go = pass_go player1

let p1_go = balance player1

let p2_go = pass_go player2

let p2_go = balance player2

let mutable_balance_tests =
  [
    balance_test "P1 starts with 1500" p1_init 1500;
    balance_test "P2 starts with 1500" p2_init 1500;
    balance_test "Add 300 to P1" x 1800;
    balance_test "Subtract 500 from P2" y 1000;
    balance_test "P1 passes GO " p1_go 2000;
    balance_test "P2 passes GO" p2_go 1200;
  ]

let player1 = acquire player1 "test_square_1"

let player2 = acquire player2 "test_square_2"

let owns_test
    (name : string)
    (player : Player.t)
    (square : Board.square_name)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (Player.owns player square)

let square_tests =
  [
    field_test "P1 acquires square 1" player1 properties
      [ "test_square_1" ];
    field_test "P2 acquires square 2" player2 properties
      [ "test_square_2" ];
    owns_test "P1 owns square 1" player1 "test_square_1" true;
    owns_test "P2 owns square 2" player2 "test_square_2" true;
    owns_test "P1 does not own square 2" player1 "test_square_2" false;
  ]

let p1_gets_square2 =
  Player.trade player1 player2 [] [ "test_square_2" ]

let player1 = fst p1_gets_square2

let player2 = snd p1_gets_square2

let trade_test =
  [
    field_test "P1 is still Jacob" player1 name "Jacob";
    field_test "P1 now acquires square 2" player1 properties
      [ "test_square_1"; "test_square_2" ];
    field_test "P2 no longer has square 2" player2 properties [];
    owns_test "P1 now owns square 2" player1 "test_square_2" true;
    owns_test "P2 now no longer owns square 2" player2 "test_square_2"
      false;
  ]

let p1_gives_everything_away =
  Player.trade player1 player2 [ "test_square_2"; "test_square_1" ] []

let player1 = fst p1_gives_everything_away

let player2 = snd p1_gives_everything_away

let trade_two_tests =
  [
    field_test "P1 now owns nothing" player1 properties [];
    field_test "P2 now owns everything" player2 properties
      [ "test_square_2"; "test_square_1" ];
    owns_test "p2 owns square 1" player2 "test_square_1" true;
  ]

let player1 = send_to_jail player1

let player2 = incr_cards player2

let player2 = incr_cards player2

let player2 = send_to_jail player2

let jail_tests =
  [
    field_test "P1 is in Jail" player1 current_square
      "Jail/Just Visiting";
    field_test "P1 is now in Jail" player1 is_in_jail true;
    field_test "P2 is not in Jail, had a jail card" player2 is_in_jail
      false;
    field_test "P1 has no get-out-of-jail cards" player1 jail_cards 0;
    field_test "P2 still has a get-out-of-jail card" player2 jail_cards
      1;
  ]

let trade_cards_p1_p2 = trade_cards player2 player1 1

let player1 = fst trade_cards_p1_p2

let player2 = snd trade_cards_p1_p2

let trade_card_tests =
  [
    field_test "P1 has a card now" player1 jail_cards 1;
    field_test "P2 has no cards" player2 jail_cards 0;
  ] *)

let suite =
  "test suite for Final project"
  >::: List.flatten
         [
           contains_compilation;
           cost_of_square_compilation;
           position_of_square_compilation;
           type_of_square_test_compilation;
           mortgage_of_square_compilation;
           set_of_square_compilation;
           upgrade_cost_compilation;
           nth_square_name_test_compilation;
           next_twelve_test_compilation;
           cost_of_tier_0_compilation;
           cost_of_tier_1_compilation;
           cost_of_tier_2_compilation;
           cost_of_tier_3_compilation;
           cost_of_tier_4_compilation;
           cost_of_tier_5_compilation;
           chance_community_compilation;
           (* field_tests;
           mutable_balance_tests;
           square_tests;
           trade_test;
           trade_two_tests;
           jail_tests; *)
         ]

let _ = run_test_tt_main suite
