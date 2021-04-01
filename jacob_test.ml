open OUnit2
open Board
open Player

(**[pp_properties lst] pretty-prints a list of properties. Not
   tail-recursive *)
let rec pp_properties = function
  | [] -> ""
  | x :: xs -> x ^ ", " ^ pp_properties xs

(**[pp_player p] pretty-prints player [p]*)
let pp_player p =
  "Name: " ^ name p ^ "\nPiece: " ^ piece p ^ "\nCurrent Square: "
  ^ current_square p ^ "\nBalance: "
  ^ string_of_int (balance p)
  ^ "\nProperties: "
  ^ pp_properties (properties p)
  ^ "\nIs bankrupt: "
  ^ string_of_bool (bankrupt p)
  ^ "\nGet out of jail cards: "
  ^ string_of_int (jail_cards p)

let player1 : Player.t = create "Jacob" "Thimble"

let player2 = create "Constantine" "Top-hat"

let field_test
    (name : string)
    (player : Player.t)
    f
    (expected_output : 'a) =
  name >:: fun _ -> assert_equal expected_output (f player)

let balance_test (name : string) (value : int) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output value ~printer:string_of_int

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
  ]

let suite =
  "test suite for Player"
  >::: List.flatten
         [
           field_tests;
           mutable_balance_tests;
           square_tests;
           trade_test;
           trade_two_tests;
           jail_tests;
         ]

let _ = run_test_tt_main suite
