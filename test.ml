open OUnit2
open Board
open Player

(*TEST PLAN: Since our system is a game, our process for testing was
  divided between extensive play-testing and OUnit testing. We used
  OUnit to test foundational modules of the sytem: Player and Board,
  whereas we play-tested the game moduels in State and main. We believe
  this demonstrates correctness of the system in that the OUnit tests
  ensure that invisible behavior of modules like Board and Player is
  correct, and play-testing thoroughly verifies that those behaviors
  interact properly in the game loop*)

(** Testing helper functions! *)
let list_printer printr lst =
  let prettify_list = function
    | "" -> "[]"
    | str -> "[" ^ String.sub str 0 (String.length str - 2) ^ "]"
  in
  let rec print_list = function
    | [] -> ""
    | h :: t -> printr h ^ "; " ^ print_list t
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
  ^ string_of_int (current_square p)
  ^ "\nBalance: "
  ^ string_of_int (balance p)
  ^ "\nProperties: "
  ^ pp_properties (properties p)
  ^ "\nIs bankrupt: "
  ^ string_of_bool (bankrupt p)
  ^ "\nGet out of jail cards: "
  ^ string_of_int (jail_cards p)

let field_test
    (name : string)
    (player : Player.t)
    f
    (expected_output : 'a) =
  name >:: fun _ -> assert_equal expected_output (f player)

let general_excp f test_name arg1 arg2 excption =
  test_name >:: fun _ -> assert_raises excption (fun () -> f arg1 arg2)

let general_test f printer test_name arg1 arg2 expected_output =
  test_name >:: fun _ ->
  assert_equal expected_output (f arg1 arg2) ~printer

let balance_test test_name value expected_output = 
  test_name >:: fun _ -> 
  assert_equal expected_output value ~printer:string_of_int

let incr_upgrade_test test_name board name expected_output =
  test_name >:: fun _ ->
  incr_upgrade board name;
  assert_equal expected_output (get_current_upgrade board name)

let get_chance_card_test test_name board expected_output =
  test_name >:: fun ctxt ->
  let card = get_chance_card board in
  assert_equal expected_output card.title ~printer:String.escaped

let get_chance_card_error test_name board excption =
  test_name >:: fun _ ->
  assert_raises excption (fun () -> get_chance_card board)

let get_community_chest_card_test test_name board expected_output =
  test_name >:: fun ctxt ->
  let card = get_community_chest_card board in
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
    general_test contains string_of_bool "valid square" basic_board
      "Pennsylvania Railroad" true;
    general_test contains string_of_bool "invalid square" basic_board
      "pennsylvania railroad" false;
  ]

let position_of_square_compilation =
  [
    general_test position_of_square
      (list_printer string_of_int)
      "valid square" basic_board "Go" [ 0 ];
    general_test position_of_square
      (list_printer string_of_int)
      "community chest" basic_board "Community Chest" [ 2; 17; 33 ];
    general_excp position_of_square "invalid square" basic_board "go"
      (UnknownSquare "go");
  ]

let cost_of_square_compilation =
  [
    general_test cost_of_square string_of_int "valid railroad"
      basic_board "Pennsylvania Railroad" 200;
    general_excp cost_of_square "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp cost_of_square "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp cost_of_square "valid railroad, no cost" faulty_board
      "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    general_excp cost_of_square "valid railroad, null cost" faulty_board
      "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_test cost_of_square string_of_int "valid street" basic_board
      "Mediterranean Avenue" 60;
    general_excp cost_of_square "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_square "valid street, no cost" faulty_board
      "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    general_excp cost_of_square "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_test cost_of_square string_of_int "income tax" basic_board
      "Income Tax" 200;
    general_excp cost_of_square "valid income tax, mismatch type"
      faulty_board "Income Tax" (UnknownType "Income tax");
    general_excp cost_of_square "valid income tax, no cost" faulty_board
      "Income Tax Bad"
      (InvalidSquare ("Income Tax", "Income Tax Bad"));
    general_excp cost_of_square "valid income tax, null cost"
      faulty_board "Income Null"
      (InvalidSquare ("Income Tax", "Income Null"));
    general_test cost_of_square string_of_int "luxury tax" basic_board
      "Luxury Tax" 75;
    general_excp cost_of_square "valid luxury tax, mismatch type"
      faulty_board "Luxury Tax" (UnknownType "Luxury tax");
    general_excp cost_of_square "valid luxury tax, no cost" faulty_board
      "Luxury Tax - Bad"
      (InvalidSquare ("Luxury Tax", "Luxury Tax - Bad"));
    general_excp cost_of_square "valid luxury tax, null cost"
      faulty_board "Luxury Null"
      (InvalidSquare ("Luxury Tax", "Luxury Null"));
    general_test cost_of_square string_of_int "water works" basic_board
      "Water Works" 150;
    general_excp cost_of_square "valid water works, mismatch type"
      faulty_board "Water Works" (UnknownType "utility");
    general_excp cost_of_square "valid water works, no cost"
      faulty_board "Water Works - Bad"
      (InvalidSquare ("Utility", "Water Works - Bad"));
    general_excp cost_of_square "valid water works, null cost"
      faulty_board "Water Null"
      (InvalidSquare ("Utility", "Water Null"));
    general_excp cost_of_square "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    general_excp cost_of_square "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_square "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_square "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_square "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_square "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
  ]

let type_of_square_test_compilation =
  [
    general_test type_of_square String.escaped "valid square"
      basic_board "Boardwalk" "Street";
    general_excp type_of_square "invalid square" basic_board "go"
      (UnknownSquare "go");
    general_excp type_of_square "invalid square" faulty_board
      "Pennsylvania Railroad" (UnknownType "Not Railroad");
  ]

let nth_square_name_test_compilation =
  [
    general_test nth_square_name String.escaped "valid square"
      basic_board 0 "Go";
    general_excp nth_square_name "failure exp" basic_board 100
      (Failure "nth");
    general_excp nth_square_name "invalid arg" basic_board (-10)
      (Invalid_argument "List.nth");
  ]

let next_twelve_test_compilation =
  [
    general_test next_twelve String.escaped "valid square - 0"
      basic_board 40
      "| Go | Mediterranean Avenue | Community Chest | Baltic Avenue | \
       Income Tax | Reading Railroad | Oriental Avenue | Chance | \
       Vermont Avenue | Connecticut Avenue | Jail/Just Visiting | St. \
       Charles Place | ";
    general_test next_twelve String.escaped "valid square - 39"
      basic_board 39
      "| Boardwalk | Go | Mediterranean Avenue | Community Chest | \
       Baltic Avenue | Income Tax | Reading Railroad | Oriental Avenue \
       | Chance | Vermont Avenue | Connecticut Avenue | Jail/Just \
       Visiting | ";
    general_excp next_twelve "invalid arg" basic_board (-10)
      (Invalid_argument "List.nth");
  ]

let mortgage_of_square_compilation =
  [
    general_test mortgage_of_square string_of_int "valid railroad"
      basic_board "Pennsylvania Railroad" 100;
    general_excp mortgage_of_square "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp mortgage_of_square "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp mortgage_of_square "valid railroad, no mortgage"
      faulty_board "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    general_excp mortgage_of_square "valid railroad, null mortgage"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_test mortgage_of_square string_of_int "valid street"
      basic_board "Mediterranean Avenue" 30;
    general_excp mortgage_of_square "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp mortgage_of_square "valid street, no mortgage"
      faulty_board "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    general_excp mortgage_of_square "valid street, null mortgage"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_test mortgage_of_square string_of_int "water works"
      basic_board "Water Works" 75;
    general_excp mortgage_of_square "valid water works, mismatch type"
      faulty_board "Water Works" (UnknownType "utility");
    general_excp mortgage_of_square "valid water works, no mortgage"
      faulty_board "Water Works - Bad"
      (InvalidSquare ("Utility", "Water Works - Bad"));
    general_excp mortgage_of_square "valid water works, null mortgage"
      faulty_board "Water Null"
      (InvalidSquare ("Utility", "Water Null"));
    general_excp mortgage_of_square "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    general_excp mortgage_of_square "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp mortgage_of_square "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp mortgage_of_square "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp mortgage_of_square "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp mortgage_of_square "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp mortgage_of_square "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp mortgage_of_square "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
  ]

let set_of_square_compilation =
  [
    general_test get_all_of_set
      (list_printer String.escaped)
      "Railroad set" basic_board "Railroad"
      [
        "Short Line";
        "B. & O. Railroad";
        "Pennsylvania Railroad";
        "Reading Railroad";
      ];
    general_test set_of_square String.escaped "valid railroad"
      basic_board "Pennsylvania Railroad" "Railroad";
    general_excp set_of_square "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp set_of_square "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp set_of_square "valid railroad, no set" faulty_board
      "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    general_excp set_of_square "valid railroad, null set" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    general_test set_of_square String.escaped "valid street" basic_board
      "Mediterranean Avenue" "Brown";
    general_excp set_of_square "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp set_of_square "valid street, no set" faulty_board
      "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    general_excp set_of_square "valid street, null set" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_test set_of_square String.escaped "water works" basic_board
      "Water Works" "Utility";
    general_excp set_of_square "valid water works, mismatch type"
      faulty_board "Water Works" (UnknownType "utility");
    general_excp set_of_square "valid water works, no set" faulty_board
      "Water Works - Bad"
      (InvalidSquare ("Utility", "Water Works - Bad"));
    general_excp set_of_square "valid water works, null set"
      faulty_board "Water Null"
      (InvalidSquare ("Utility", "Water Null"));
    general_excp set_of_square "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    general_excp set_of_square "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp set_of_square "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp set_of_square "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp set_of_square "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    general_excp set_of_square "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp set_of_square "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp set_of_square "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
  ]

let upgrade_compilation =
  [
    general_test upgrade_cost string_of_int "valid street" basic_board
      "Mediterranean Avenue" 50;
    general_excp upgrade_cost "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    general_excp upgrade_cost "valid street, no upgrade cost"
      faulty_board "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    general_excp upgrade_cost "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp upgrade_cost "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    general_excp upgrade_cost "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp upgrade_cost "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp upgrade_cost "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp upgrade_cost "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    general_excp upgrade_cost "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp upgrade_cost "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp upgrade_cost "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    general_excp upgrade_cost "invalid type - railroad" basic_board
      "Reading Railroad" (TypeMismatch "Railroad");
    general_excp upgrade_cost "invalid type - utility" basic_board
      "Water Works" (TypeMismatch "Utility");
    general_test get_current_upgrade string_of_int "valid street"
      basic_board "Mediterranean Avenue" 0;
    general_test get_current_upgrade string_of_int "valid railroad"
      basic_board "Reading Railroad" 0;
    general_excp get_current_upgrade "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp get_current_upgrade "valid street, no upgrade number"
      faulty_board "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    general_excp get_current_upgrade "valid railroad, no upgrade number"
      faulty_board "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    general_excp get_current_upgrade "valid street, null upgrade number"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp get_current_upgrade "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp get_current_upgrade "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp get_current_upgrade "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp get_current_upgrade "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp get_current_upgrade "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp get_current_upgrade "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp get_current_upgrade "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp get_current_upgrade "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp get_current_upgrade "invalid type - utility"
      basic_board "Water Works" (TypeMismatch "Utility");
    incr_upgrade_test "valid street" basic_board "Mediterranean Avenue"
      1;
    incr_upgrade_test "valid railroad" basic_board "Reading Railroad" 1;
    general_excp incr_upgrade "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    general_excp incr_upgrade "valid street, no upgrade number"
      faulty_board "Baltic Avenue - Bad"
      (InvalidSquare ("Street", "Baltic Avenue - Bad"));
    general_excp incr_upgrade "valid railroad, no upgrade number"
      faulty_board "Reading Railroad - Bad"
      (InvalidSquare ("Railroad", "Reading Railroad - Bad"));
    general_excp incr_upgrade "valid street, null upgrade number"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp incr_upgrade "maxupgradereached" faulty_board
      "Short Line" MaxUpgradeReached;
    general_excp incr_upgrade "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    general_excp incr_upgrade "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp incr_upgrade "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp incr_upgrade "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp incr_upgrade "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    general_excp incr_upgrade "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp incr_upgrade "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp incr_upgrade "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    general_excp incr_upgrade "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_rent_compilation =
  [
    general_test cost_of_rent string_of_int "valid railroad" basic_board
      "Pennsylvania Railroad" 25;
    general_test cost_of_rent string_of_int "valid street" basic_board
      "Virginia Avenue" 12;
    general_excp cost_of_rent "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp cost_of_rent "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp cost_of_rent "valid railroad, no rent tiers period"
      faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_excp cost_of_rent "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    general_excp cost_of_rent "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    general_excp cost_of_rent "valid railroad, null tier" faulty_board
      "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    general_excp cost_of_rent "valid railroad, null cost" faulty_board
      "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    general_test cost_of_rent string_of_int "valid street" basic_board
      "Mediterranean Avenue" 10;
    general_excp cost_of_rent "valid street, mismatch type" faulty_board
      "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_rent "valid street, no rent tiers period"
      faulty_board "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_rent "valid street, null tier" faulty_board
      "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_rent "valid street, null cost" faulty_board
      "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_rent "invalid type - go" basic_board "Go"
      (TypeMismatch "Go");
    general_excp cost_of_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_rent "invalid type - free parking" basic_board
      "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_rent "invalid type - go to jail" basic_board
      "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_rent "invalid type - luxury tax" basic_board
      "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_rent "invalid type - income tax" basic_board
      "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_0_compilation =
  [
    general_test cost_of_tier_0_rent string_of_int "valid railroad"
      basic_board "Pennsylvania Railroad" 25;
    general_excp cost_of_tier_0_rent "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp cost_of_tier_0_rent "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp cost_of_tier_0_rent
      "valid railroad, no rent tiers period" faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_excp cost_of_tier_0_rent "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    general_excp cost_of_tier_0_rent "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    general_excp cost_of_tier_0_rent "valid railroad, null tier"
      faulty_board "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    general_excp cost_of_tier_0_rent "valid railroad, null cost"
      faulty_board "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    general_test cost_of_tier_0_rent string_of_int "valid street"
      basic_board "Mediterranean Avenue" 2;
    general_excp cost_of_tier_0_rent "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_tier_0_rent
      "valid street, no rent tiers period" faulty_board
      "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_tier_0_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_tier_0_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_tier_0_rent "valid street, null tier"
      faulty_board "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_tier_0_rent "valid street, null cost"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_tier_0_rent "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp cost_of_tier_0_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_tier_0_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_tier_0_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_tier_0_rent "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_tier_0_rent "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_tier_0_rent "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_tier_0_rent "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_tier_0_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_1_compilation =
  [
    general_test cost_of_tier_1_rent string_of_int "valid railroad"
      basic_board "Pennsylvania Railroad" 50;
    general_excp cost_of_tier_1_rent "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp cost_of_tier_1_rent "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp cost_of_tier_1_rent
      "valid railroad, no rent tiers period" faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_excp cost_of_tier_1_rent "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    general_excp cost_of_tier_1_rent "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    general_excp cost_of_tier_1_rent "valid railroad, null tier"
      faulty_board "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    general_excp cost_of_tier_1_rent "valid railroad, null cost"
      faulty_board "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    general_test cost_of_tier_1_rent string_of_int "valid street"
      basic_board "Mediterranean Avenue" 10;
    general_excp cost_of_tier_1_rent "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_tier_1_rent
      "valid street, no rent tiers period" faulty_board
      "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_tier_1_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_tier_1_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_tier_1_rent "valid street, null tier"
      faulty_board "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_tier_1_rent "valid street, null cost"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_tier_1_rent "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp cost_of_tier_1_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_tier_1_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_tier_1_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_tier_1_rent "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_tier_1_rent "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_tier_1_rent "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_tier_1_rent "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_tier_1_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_2_compilation =
  [
    general_test cost_of_tier_2_rent string_of_int "valid railroad"
      basic_board "Pennsylvania Railroad" 100;
    general_excp cost_of_tier_2_rent "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp cost_of_tier_2_rent "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp cost_of_tier_2_rent
      "valid railroad, no rent tiers period" faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_excp cost_of_tier_2_rent "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    general_excp cost_of_tier_2_rent "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    general_excp cost_of_tier_2_rent "valid railroad, null tier"
      faulty_board "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    general_excp cost_of_tier_2_rent "valid railroad, null cost"
      faulty_board "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    general_test cost_of_tier_2_rent string_of_int "valid street"
      basic_board "Mediterranean Avenue" 30;
    general_excp cost_of_tier_2_rent "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_tier_2_rent
      "valid street, no rent tiers period" faulty_board
      "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_tier_2_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_tier_2_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_tier_2_rent "valid street, null tier"
      faulty_board "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_tier_2_rent "valid street, null cost"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_tier_2_rent "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp cost_of_tier_2_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_tier_2_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_tier_2_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_tier_2_rent "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_tier_2_rent "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_tier_2_rent "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_tier_2_rent "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_tier_2_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_3_compilation =
  [
    general_test cost_of_tier_3_rent string_of_int "valid railroad"
      basic_board "Pennsylvania Railroad" 200;
    general_excp cost_of_tier_3_rent "invalid railroad" basic_board
      "pennsylvania railroad" (UnknownSquare "pennsylvania railroad");
    general_excp cost_of_tier_3_rent "valid railroad, mismatch type"
      faulty_board "Pennsylvania Railroad" (UnknownType "Not Railroad");
    general_excp cost_of_tier_3_rent
      "valid railroad, no rent tiers period" faulty_board "Short Line"
      (InvalidSquare ("Railroad", "Short Line"));
    general_excp cost_of_tier_3_rent "valid railroad, no cost period"
      faulty_board "Big Bertha"
      (InvalidSquare ("Railroad", "Big Bertha"));
    general_excp cost_of_tier_3_rent "valid railroad, no tier period"
      faulty_board "Thomas"
      (InvalidSquare ("Railroad", "Thomas"));
    general_excp cost_of_tier_3_rent "valid railroad, null tier"
      faulty_board "B. & O. Railroad"
      (InvalidSquare ("Railroad", "B. & O. Railroad"));
    general_excp cost_of_tier_3_rent "valid railroad, null cost"
      faulty_board "Reading Railroad"
      (InvalidSquare ("Railroad", "Reading Railroad"));
    general_test cost_of_tier_3_rent string_of_int "valid street"
      basic_board "Mediterranean Avenue" 90;
    general_excp cost_of_tier_3_rent "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_tier_3_rent
      "valid street, no rent tiers period" faulty_board
      "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_tier_3_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_tier_3_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_tier_3_rent "valid street, null tier"
      faulty_board "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_tier_3_rent "valid street, null cost"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_tier_3_rent "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp cost_of_tier_3_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_tier_3_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_tier_3_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_tier_3_rent "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_tier_3_rent "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_tier_3_rent "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_tier_3_rent "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_tier_3_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
  ]

let cost_of_tier_4_compilation =
  [
    general_test cost_of_tier_4_rent string_of_int "valid street"
      basic_board "Mediterranean Avenue" 160;
    general_excp cost_of_tier_4_rent "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_tier_4_rent
      "valid street, no rent tiers period" faulty_board
      "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_tier_4_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_tier_4_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_tier_4_rent "valid street, null tier"
      faulty_board "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_tier_4_rent "valid street, null cost"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_tier_4_rent "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp cost_of_tier_4_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_tier_4_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_tier_4_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_tier_4_rent "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_tier_4_rent "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_tier_4_rent "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_tier_4_rent "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_tier_4_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
    general_excp cost_of_tier_4_rent "invalid type - railroad"
      basic_board "Reading Railroad" (TypeMismatch "Railroad");
  ]

let cost_of_tier_5_compilation =
  [
    general_test cost_of_tier_5_rent string_of_int "valid street"
      basic_board "Mediterranean Avenue" 250;
    general_excp cost_of_tier_5_rent "valid street, mismatch type"
      faulty_board "Mediterranean Avenue" (UnknownType "Streets");
    general_excp cost_of_tier_5_rent
      "valid street, no rent tiers period" faulty_board
      "Connecticut Avenue"
      (InvalidSquare ("Street", "Connecticut Avenue"));
    general_excp cost_of_tier_5_rent "valid street, no cost period"
      faulty_board "Oriental Avenue"
      (InvalidSquare ("Street", "Oriental Avenue"));
    general_excp cost_of_tier_5_rent "valid street, no tier period"
      faulty_board "Vermont Avenue"
      (InvalidSquare ("Street", "Vermont Avenue"));
    general_excp cost_of_tier_5_rent "valid street, null tier"
      faulty_board "Boardwalk"
      (InvalidSquare ("Street", "Boardwalk"));
    general_excp cost_of_tier_5_rent "valid street, null cost"
      faulty_board "Park Place"
      (InvalidSquare ("Street", "Park Place"));
    general_excp cost_of_tier_5_rent "invalid type - go" basic_board
      "Go" (TypeMismatch "Go");
    general_excp cost_of_tier_5_rent "invalid type - jail/just visiting"
      basic_board "Jail/Just Visiting"
      (TypeMismatch "Jail/Just Visiting");
    general_excp cost_of_tier_5_rent "invalid type - chance" basic_board
      "Chance" (TypeMismatch "Chance");
    general_excp cost_of_tier_5_rent "invalid type - community chest"
      basic_board "Community Chest" (TypeMismatch "Community Chest");
    general_excp cost_of_tier_5_rent "invalid type - free parking"
      basic_board "Free Parking" (TypeMismatch "Free Parking");
    general_excp cost_of_tier_5_rent "invalid type - go to jail"
      basic_board "Go to Jail" (TypeMismatch "Go to Jail");
    general_excp cost_of_tier_5_rent "invalid type - luxury tax"
      basic_board "Luxury Tax" (TypeMismatch "Luxury Tax");
    general_excp cost_of_tier_5_rent "invalid type - income tax"
      basic_board "Income Tax" (TypeMismatch "Income Tax");
    general_excp cost_of_tier_5_rent "invalid type - go" basic_board
      "Water Works" (TypeMismatch "Utility");
    general_excp cost_of_tier_5_rent "invalid type - railroad"
      basic_board "Reading Railroad" (TypeMismatch "Railroad");
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

let field_tests =
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
    balance_test "Subtract 500 from\n   P2" y 1000;
    balance_test "P1 passes GO " p1_go 2000;
    balance_test "P2\n   passes GO" p2_go 1200;
  ]

let trade_cards_p1_p2 = trade_cards player2 player1 1

let player1 = fst trade_cards_p1_p2

let player2 = snd trade_cards_p1_p2

let trade_card_tests =
  [
    field_test "P1 has a card now" player1 jail_cards 1;
    field_test "P2 has no cards" player2 jail_cards 0;
  ]

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
           upgrade_compilation;
           nth_square_name_test_compilation;
           next_twelve_test_compilation;
           cost_of_rent_compilation;
           cost_of_tier_0_compilation;
           cost_of_tier_1_compilation;
           cost_of_tier_2_compilation;
           cost_of_tier_3_compilation;
           cost_of_tier_4_compilation;
           cost_of_tier_5_compilation;
           chance_community_compilation;
           field_tests;
           mutable_balance_tests;
         ]

let _ = run_test_tt_main suite
