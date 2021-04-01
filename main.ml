open ANSITerminal

(** [get_player_one st] takes in the user input string and prints a
    string as confirmation of that input being assigned to Player 1. *)
let get_player_one st =
  match read_line () with
  | exception End_of_file -> "No player"
  | player_1_name -> "Player 1: " ^ player_1_name

(** [get_player_two st] takes in the user input string and prints a
    string as confirmation of that input being assigned to Player 2. *)
let get_player_two st =
  match read_line () with
  | exception End_of_file -> "No player"
  | player_2_name -> "Player 2: " ^ player_2_name

(** [turn_printer b st] prints the name of [player1] in [st] if [b] is
    [true]. If [b] is [false], the name of [player2] in [st] will be
    printed. *)
let turn_printer b st =
  if b = true then print_endline (State.get_player1 st ^ "'s turn")
  else print_endline (State.get_player2 st ^ "'s turn")

(** [take_turn p1 p2 b] switches the state from being player 1's turn to
    player 2's turn or vice versa. *)
let take_turn p1 p2 b = State.switch_turns p1 p2 (not b)

(** [rolling b st] is a recursive function that outputs a random number
    between 1 and 6 if the user inputs a "y" and then changes the state
    so it is the other player's turn. If the user inputs a "q", the game
    is exited. If the user inputs anything other than "y" or "q", the
    state remains unchanged. *)
let rec rolling b st =
  print_endline "Roll die? [Y/N]";
  match read_line () with
  | "y" | "Y" ->
      print_endline (Roll.dice () |> string_of_int);
      turn_printer (not b)
        (take_turn (State.get_player1 st) (State.get_player2 st) true);
      rolling (not b) st
  | "q" | "Q" -> exit 0
  | _ ->
      print_endline "Press y to roll die or q to quit";
      turn_printer b
        (take_turn (State.get_player1 st) (State.get_player2 st) false);
      rolling b st

(** [main ()] prompts the user to input the names of the two players,
    and then starts the game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\nWelcome to Monopoly\n";
  print_endline "Please enter the name of Player 1.\n";
  let plyr1 = get_player_one State.init_state in
  print_endline plyr1;
  print_endline "Please enter the name of Player 2.\n";
  let plyr2 = get_player_two State.init_state in
  print_endline plyr2;

  turn_printer true (take_turn plyr1 plyr2 true);
  rolling true (take_turn plyr1 plyr2 true)

let () = main ()
