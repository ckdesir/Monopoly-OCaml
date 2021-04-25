open ANSITerminal
open Board

let make_board board_json =
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

let get_balance plyr = Player.balance plyr

(** [turn_printer b st] prints the name of [player1] in [st] if [b] is
    [true]. If [b] is [false], the name of [player2] in [st] will be
    printed. *)
let turn_printer st =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (Player.name (State.get_current_player st) ^ "'s turn\n")

(** [take_turn s] switches the state from being player 1's turn to
    player 2's turn or vice versa. *)
let take_turn st = State.switch_turns st

(** Extend this to cover railroads/utiltiis eventually, pattern match
    against the type and carry out respective operations, exhaustive
    pattern match is a recheck of input*)
let rec check_property play n st =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\nPlease type the name of a valid property on the board!\n";
  match read_line () with
  | exception End_of_file -> ()
  | "q" | "Q" -> play n st
  | prop ->
      let board = State.get_board st in
      if
        Board.contains board prop
        && Board.type_of_square board prop = "Street"
      then begin
        print_endline "----------------------------------------";
        ANSITerminal.print_string [ ANSITerminal.red ]
          ("Name: " ^ prop ^ "\n");
        ANSITerminal.print_string [ ANSITerminal.green ]
          ( "Initial cost: "
          ^ string_of_int (Board.cost_of_square board prop)
          ^ "\n" );
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          ("Set: " ^ Board.set_of_square board prop ^ "\n");
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ( "Cost of house/hotel: "
          ^ string_of_int (Board.upgrade_cost board prop)
          ^ "\n" );
        ( match State.get_who_owns st prop with
        | Some person ->
            ANSITerminal.print_string
              [ ANSITerminal.magenta ]
              ("Owned by: " ^ person)
        | None ->
            ANSITerminal.print_string [ ANSITerminal.magenta ] "Unowned"
        );
        print_newline ();
        print_endline "----------------------------------------";
        print_endline "Do you want to check out another property? [Y/N]";
        match read_line () with
        | "y" | "Y" -> check_property play n st
        | _ ->
            Unix.sleepf 0.5;
            print_newline ();
            play n st
      end
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nCheck input, invalid square. Try again. \n";
      check_property play n st

(** Maybe also print out how many they own in the set? + Add in mortgage
    process / bankrupcy process *)
let rec handle_property st current_square_name board =
  let current_player = ref (State.get_current_player st) in
  match State.get_who_owns st current_square_name with
  | Some person -> (
    let cost = Board.get_current_upgrade board current_square_name in
    ()
  )
  | None -> (
      let cost = Board.cost_of_square board current_square_name in
      ANSITerminal.print_string [ ANSITerminal.green ]
        ( "This property is unowned. Would you like to buy it? It has \
           a cost of " ^ string_of_int cost
        ^ ". And your current balance is: "
        ^ string_of_int (Player.balance !current_player) ^ ".\n" );
      match read_line () with
      | "y" | "Y" -> (
          try
            Player.bank_transaction (-cost) !current_player;
            current_player :=
              Player.acquire !current_player current_square_name;
            print_newline ();
            ANSITerminal.print_string [ ANSITerminal.green ]
              ( "Congrats! You are the brand new owner of "
              ^ current_square_name ^ "! Your current balance is now: "
              ^ string_of_int (Player.balance !current_player) );
            State.change_current_player st !current_player
          with Player.InsufficientFunds ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              ( "Sorry you do not have the sufficient funds. Your \
                 current balance is: "
              ^ string_of_int (Player.balance !current_player) ) )
      | "n" | "N" -> ()
      | _ -> handle_property st current_square_name board )

let handle_go_to_jail st =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Sorry! You are now being sent straight to jail, criminal!";
  State.send_curr_jail st

(** Add in mortaging process / bakruptcy process etc. *)
let handle_tax st current_square_name board =
  let cost = Board.cost_of_square board current_square_name in
  let current_player = ref (State.get_current_player st) in
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("You owe $" ^ string_of_int cost ^ " to the bank. Pay up now!");
  try Player.bank_transaction (-cost) !current_player
  with Player.InsufficientFunds ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      ( "Sorry you do not have the sufficient funds. Your current \
         balance is: "
      ^ string_of_int (Player.balance !current_player) )

(* If not in jail *)
(* Check to see if third double, send immediately to jail and end turn,
   otherwise carry out respective roll *)
(* If in jail, check to see if double then move them out, and carry out
   respective roll, make sure they do not roll again. *)

(** Not to sure of the jail rules, need to research. Not sure if you can
    roll again if you pay to get out and roll a dobule. *)
let rec roll play n st =
  let board = State.get_board st in
  let first_roll = Roll.dice () in
  let second_roll = Roll.dice () in
  if first_roll = second_roll then
    Player.incr_doubles (State.get_current_player st)
  else Player.clear_doubles (State.get_current_player st);
  let total_roll = first_roll + second_roll in
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    ( "You have rolled a "
    ^ (first_roll |> string_of_int)
    ^ " and a "
    ^ (second_roll |> string_of_int)
    ^ "\n" );

  let was_in_jail = State.is_in_jail st in

  if State.is_in_jail st then
    if Player.doubles (State.get_current_player st) = 1 then (
      ANSITerminal.print_string [ ANSITerminal.green ] "You're free!\n";
      Player.change_jail_status (State.get_current_player st)
      |> State.change_current_player st )
    else
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Sorry, you're still in jail\n"
  else if Player.doubles (State.get_current_player st) >= 3 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Sorry, you've rolled three doubles in a row! You are now being \
       sent to jail criminal!";
    State.send_curr_jail st );

  if not (State.is_in_jail st) then begin
    State.move_current_player st total_roll;
    let current_square_name =
      Board.nth_square_name board
        (Player.current_square (State.get_current_player st))
    in
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ("You have landed on: " ^ current_square_name ^ "\n");
    match Board.type_of_square board current_square_name with
    | "Chance" -> ()
    | "Community Chest" -> ()
    | "Street" -> handle_property st current_square_name board
    | "Income Tax" -> handle_tax st current_square_name board
    | "Luxury Tax" -> handle_tax st current_square_name board
    | "Railroad" -> handle_property st current_square_name board
    | "Go to Jail" -> handle_go_to_jail st
    | "Utility" -> handle_property st current_square_name board
    | "Free Parking" -> ()
    | _ -> ()
  end;

  if
    (not (State.is_in_jail st))
    && first_roll = second_roll
    && not was_in_jail
  then begin
    print_newline ();
    turn_printer st;
    print_newline ();
    Unix.sleepf 0.5;
    roll play n st
  end
  else begin
    print_newline ();
    print_newline ();
    let new_state = take_turn st in
    turn_printer new_state;
    Unix.sleepf 0.5;
    play ((n + 1) mod State.get_num_players st) new_state
  end

let quit play n st =
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
      Unix.sleep 2;
      play n st

let check_board play n st =
  let next_twelve =
    Board.next_twelve (State.get_board st)
      (Player.current_square (State.get_current_player st))
  in
  let length = String.index_from next_twelve 1 '|' in
  ANSITerminal.print_string [ ANSITerminal.red ]
    (String.sub next_twelve 0 (length + 1));
  print_endline
    (String.sub next_twelve (length + 1)
       (String.length next_twelve - length - 1));
  print_newline ();
  Unix.sleep 3;
  turn_printer st;
  print_newline ();
  play n st

(** Check to see if they own any sets, print out the sets they own or kick them out 
  of this option if not, match read_line() of set name, print out streets,
    another match_readline() for appropriate street, upgrade according to *)
let buy_sell_buildings play n st = 
  play n st

let rec play n st =
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
  print_endline "\t[6] - Check your balance";
  print_endline "\t[Q] - To quit";
  print_endline "----------------------------------------";
  ANSITerminal.print_string [ ANSITerminal.white ] "Option: ";
  match read_line () with
  | "r" | "R" -> roll play n st
  | "1" -> ()
  | "2" -> ()
  | "3" -> buy_sell_buildings play n st 
  | "4" -> check_board play n st
  | "5" -> check_property play n st
  | "6" ->
      let current = State.get_current_player st in
      print_endline (string_of_int (get_balance current))
  | "q" | "Q" -> quit play n st
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] "\nCheck input\n\n";
      turn_printer st;
      print_newline ();
      play n st

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
    let state =
      State.init_state players (make_board "basic_board.json")
    in
    play 0 state;
    ()
  with Failure "int_of_string" ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Bad input! Please make sure to only put letters and numbers \
       when appropriate.";
    main ()

let () = main ()
