open ANSITerminal
open Board

let play board_json =
  board_json |> Yojson.Basic.from_file |> Board.from_json

let get_num_player () =
  match read_int () with
  | exception End_of_file -> 0
  | num_of_players ->
      print_newline ();
      num_of_players

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
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (Player.name (State.get_current_player st) ^ "'s turn\n")

(** [take_turn s] switches the state from being player 1's turn to
    player 2's turn or vice versa. *)
let take_turn st = State.switch_turns st
let rec rolling n st =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Would you like to:\n";
  (* if Player.is_in_jail (State.get_current_player st) then
     print_endline "----------------------------------------";
     print_endline "\t[R] - Roll"; print_endline "\t[1] - Trade";
     print_endline "\t[2] - Mortgage/Unmortgage properties";
     print_endline "\t[3] - Buy/Sell houses/hotels"; print_endline
     "\t[4] - Check board"; print_endline "\t[J] - Get out of jail ($50
     fine/GET OUT OF JAIL FREE card)"; print_endline "\t[Q] - To quit";
     print_endline "----------------------------------------"; else *)
  print_endline "----------------------------------------";
  print_endline "\t[R] - Roll";
  print_endline "\t[1] - Trade";
  print_endline "\t[2] - Mortgage/Unmortgage properties";
  print_endline "\t[3] - Buy/Sell houses/hotels";
  print_endline "\t[4] - Check board";
  print_endline "\t[5] - Check a property";
  print_endline "\t[Q] - To quit";
  print_endline "----------------------------------------";
  match read_line () with
  | "r" | "R" ->
      let first_roll = Roll.dice () in
      let second_roll = Roll.dice () in
      let total_roll = first_roll + second_roll in
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        ( "You have rolled a "
        ^ (first_roll |> string_of_int)
        ^ " and a "
        ^ (second_roll |> string_of_int) );
      (* If not in jail *)
      (* Check to see if third double, send immediately to jail and end turn, otherwise carry out respective roll *)
      (* If in jail, check to see if double then move them out, and carry out respective roll, make sure they do not roll again. *)
      let new_state = take_turn st in
      turn_printer new_state;
      rolling ((n + 1) mod State.get_num_players st) new_state
  | "1" -> ()
  | "2" -> ()
  | "3" -> ()
  | "4" -> print_endline (Board.next_twelve (State.get_board st) (Player.current_square (State.get_current_player st)));
  | "5" -> "Checks the stats of a property (aka current houses built, who owns, set, prices, etc) Pretty-prints a property basically"; ()
  | "q" | "Q" -> (
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "\nAre you sure you want to quit? [Y/N]\n";
      match read_line () with
      | "y" | "Y" ->
          exit 0;
          ()
      | _ ->
          print_newline ();
          turn_printer st;
          print_newline ();
          rolling n st )
  | _ ->
      turn_printer st;
      print_newline ();
      rolling n st

(** [main ()] prompts the user to input the names of the two players,
    and then starts the game. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\nWelcome to Monopoly\n";
  print_endline
    "How many people are playing today? (Please enter only a number)";
  try
    let num_of_players = get_num_player () in
    let players =
      Array.make num_of_players (Player.create "Jacob" "Horse")
    in
    for i = 0 to num_of_players - 1 do
      print_endline
        ( "Please enter the name of Player "
        ^ string_of_int (i + 1)
        ^ "." );
      let player = get_player () in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("\nPlayer " ^ string_of_int (i + 1) ^ ": " ^ player ^ "\n");
      print_endline "Choose a piece";
      let piece = get_piece () in
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("\nPlayer " ^ string_of_int (i + 1) ^ " piece: " ^ piece ^ "\n");
      players.(i) <- Player.create player piece
    done;
    print_newline ();
    let state = State.init_state players (play "basic_board.json") in
    rolling 0 state;
    ()
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Bad input! Please make sure to only put letters and numbers \
       when appropriate.";
    main ()

let () = main ()
