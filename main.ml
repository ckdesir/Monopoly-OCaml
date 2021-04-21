open ANSITerminal
open Board

let play board_json =
  board_json |> Yojson.Basic.from_file |> Board.from_json

let get_num_player () =
  match read_line () with
  | exception End_of_file -> 0
  | num_of_players -> int_of_string num_of_players

let get_player () =
  match read_line () with
  | exception End_of_file -> "No player"
  | name -> name

let get_piece () =
  match read_line () with
  | exception End_of_file -> "Choose a piece"
  | piece -> piece

(** [turn_printer b st] prints the name of [player1] in [st] if [b] is
    [true]. If [b] is [false], the name of [player2] in [st] will be
    printed. *)
let turn_printer st =
  print_endline (Player.name (State.get_current_player st) ^ "'s turn")

(** [take_turn s] switches the state from being player 1's turn to
    player 2's turn or vice versa. *)
let take_turn st = State.switch_turns st

(** [rolling n st] is a recursive function that outputs a random number
    between 1 and 6 if the user inputs a "y" and then changes the state
    so it is the next player's turn. If the user inputs a "q", the game
    is exited. If the user inputs anything other than "y" or "q", the
    state remains unchanged. [n] is the current player's int *)
let rec rolling n st =
  print_endline "Roll die? [Y/N]";
  match read_line () with
  | "y" | "Y" ->
      print_endline (Roll.dice () |> string_of_int);
      let new_state = take_turn st in
      turn_printer new_state;
      rolling ((n + 1) mod State.get_num_players st) new_state
  | "q" | "Q" ->
      exit 0;
      ()
  | _ ->
      print_endline "Press y to roll die or q to quit";
      turn_printer (take_turn st);
      rolling n st

(** [main ()] prompts the user to input the names of the two players,
    and then starts the game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\nWelcome to Monopoly\n";
  print_endline "How many people are playing today?\n";
  let num_of_players = get_num_player () in
  let players =
    Array.make num_of_players (Player.create "Jacob" "Horse")
  in
  for i = 0 to num_of_players - 1 do
    print_endline
      ("Please enter the name of Player " ^ string_of_int (i + 1) ^ ".");
    let player = get_player () in
    print_endline
      ("\nPlayer " ^ string_of_int (i + 1) ^ ": " ^ player ^ "\n");
    print_endline "Choose a piece";
    let piece = get_piece () in
    print_endline
      ("\nPlayer " ^ string_of_int (i + 1) ^ " piece: " ^ piece ^ "\n");
    players.(i) <- Player.create player piece
  done;

  let state =
    State.init_state (Array.to_list players) (play "basic_board.json")
  in
  rolling 0 state;
  ()

let () = main ()
