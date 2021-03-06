open ANSITerminal
open Board

(** [make_board board_json] converts the json file of the board into a
    Board.t object. *)
let make_board board_json =
  board_json |> Yojson.Basic.from_file |> Board.from_json

(** [get_num_player ()] takes the user input for number of players and
    stores it in variable [num_of_players] *)
let rec get_num_player () =
  match read_int () with
  | exception End_of_file -> 0
  | 0 ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Please enter a number larger than 0.\n";
      get_num_player ()
  | num_of_players ->
      print_newline ();
      num_of_players

(** [get_player ()] reads the user input for the player name and stores
    it in variable [name] *)
let get_player () =
  match read_line () with
  | exception End_of_file -> "No player"
  | name -> name

(** [get_piece ()] reads the user input for the name of their piece and
    stores it in variable [piece] *)
let get_piece () =
  match read_line () with
  | exception End_of_file -> "Choose a piece"
  | piece -> piece

(** [get_balance plyr] returns the amount of money in dollars that
    player [plyr] currently has. *)
let get_balance plyr = Player.balance plyr

(** [turn_printer st] prints the name of the player whose turn it
    currently is in state [st]. *)
let turn_printer st =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (Player.name (State.get_current_player st) ^ "'s turn\n")

(** [take_turn st] switches the state from one player's turn to the next
    player's turn in [st]. *)
let take_turn st = State.switch_turns st

(** [check_property play n st] prompts the user to enter the name of a
    property on the board, then prints the information about that
    property, including its name, how much it costs to buy it, what set
    it belongs to, and how much it costs to put houses and hotels on it. *)
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
        name_helper prop;
        initial_cost_helper board prop;
        set_helper board prop;
        hotel_cost_helper board prop;
        (match State.get_who_owns st prop with
        | Some player -> owner_helper player
        | None ->
            ANSITerminal.print_string
              [ ANSITerminal.magenta ]
              "Unowned\n");
        print_endline "----------------------------------------";
        print_endline "Do you want to check out another property? [Y/N]";
        another_prop_helper play n st
      end
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nCheck input, invalid square. Try again. \n";
      check_property play n st

(** [name_helper prop] prints [prop], labeled as the name of that
    property. *)
and name_helper prop =
  ANSITerminal.print_string [ ANSITerminal.red ] ("Name: " ^ prop ^ "\n")

(** [initial_cost_helper board prop] prints the cost of [prop]. *)
and initial_cost_helper board prop =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Initial cost: "
    ^ string_of_int (Board.cost_of_square board prop)
    ^ "\n")

(** [set_helper board prop] prints the set that [prop] belongs to on
    board [board] *)
and set_helper board prop =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ("Set: " ^ Board.set_of_square board prop ^ "\n")

(** [hotel_cost_helper board prop] prints the amount that it costs to
    put houses and hotels on property [prop] on board [board] *)
and hotel_cost_helper board prop =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Cost of house/hotel: "
    ^ string_of_int (Board.upgrade_cost board prop)
    ^ "\n")

(** [owner_helper player] prints the name of the player [player] *)
and owner_helper player =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    ("Owned by: " ^ Player.name player)

(** [another_prop_helper play n st] calls [check_property] again if the
    player inputs that they would like to check another property. *)
and another_prop_helper play n st =
  match read_line () with
  | "y" | "Y" -> check_property play n st
  | _ ->
      Unix.sleepf 0.5;
      print_newline ();
      play n st

(** [trade_prop play n st] reads the user input for which property they
    would like to trade, which player they would like to trade with, and
    for what amount they would like to trade the property for. *)
let rec trade_prop play n st =
  print_endline "Please enter property to trade";
  match read_line () with
  | exception End_of_file -> ()
  | prop ->
      if not (Player.owns (State.get_current_player st) prop) then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Sorry, it appears that property isn't yours to give away...";
        turn_printer st;
        play n st)
      else
        print_endline "What number player would you like to trade with?";
      let plyr = int_of_string (read_line ()) - 1 in
      print_endline
        "How much would you like to trade this property for?";
      let amount = int_of_string (read_line ()) in
      ask_player_to_trade plyr amount prop st play

(** [ask_player_to_trade plyr amt prop st play] asks the player
    specified by the user input in [trade_prop play n st] if they would
    like to trade the specified amount of money for the specified
    property. If this player says yes, the state of the game changes so
    that they are now the owner of that property. If they say no,
    gameplay continues as normal. *)
and ask_player_to_trade plyr amt prop st play =
  let giver = State.get_current_player st in
  print_endline
    (Player.name (State.get_player plyr st)
    ^ ", would you like to trade " ^ string_of_int amt
    ^ " to have ownership of " ^ prop ^ "?");
  match read_line () with
  | "y" | "Y" ->
      handle_trade plyr giver amt prop st play;
      play (State.get_turn st) st
      (* make plyr the owner, make old player not the owner *)
  | "n" | "N" -> play (State.get_turn st) st
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] "Bad input";
      ask_player_to_trade plyr amt prop st play

(** [handle_trade recip_index giver amt prop st play] does the actual
    changing of the state when the player trading indicates that they
    would like to trade. *)
and handle_trade recip_index giver amt prop st play =
  let recip = State.get_player recip_index st in
  (try Player.pay amt recip (State.get_current_player st)
   with Player.InsufficientFunds ->
     ANSITerminal.print_string [ ANSITerminal.red ]
       "Sorry, you can't afford this offer";
     turn_printer st;
     play (State.get_turn st) st);
  State.change_current_player st (Player.remove_props giver [ prop ]);
  State.change_player_at recip_index st
    (Player.add_to_properties recip prop);
  turn_printer st;
  play (State.get_turn st) st

(** [handle_property st current_square_name board roll] checks the
    ownership status of square [current_square_name] and gives the
    current player the chance to buy it if it is unowned. *)
let rec handle_property st current_square_name board roll =
  let rec expon n e =
    if e = 0 then 1 else if e = 1 then n else n * expon n (e - 1)
  in
  let current_player = ref (State.get_current_player st) in
  let cost = ref 0 in
  match State.get_who_owns st current_square_name with
  | Some player ->
      player_helper player current_player board current_square_name cost
        expon st roll
  | None -> (
      let cost = Board.cost_of_square board current_square_name in
      ANSITerminal.print_string [ ANSITerminal.green ]
        ("This property is unowned. Would you like to buy it? It has a \
          cost of " ^ string_of_int cost
       ^ " and your current balance is: "
        ^ string_of_int (Player.balance !current_player)
        ^ ". [Y/N]\n");
      match read_line () with
      | "y" | "Y" ->
          handle_buying cost current_player st current_square_name
      | "n" | "N" -> ()
      | _ -> handle_property st current_square_name board roll)

and player_helper
    player
    current_player
    board
    current_square_name
    cost
    expon
    st
    roll =
  if player <> !current_player then (
    if Board.type_of_square board current_square_name = "Street" then (
      cost := Board.cost_of_rent board current_square_name;
      let set_name = Board.set_of_square board current_square_name in
      if
        State.is_set_owned st set_name
        && Board.get_current_upgrade board current_square_name = 0
      then cost := 2 * !cost)
    else if Board.type_of_square board current_square_name = "Railroad"
    then cost := 25 * expon 2 (Player.railroads player - 1)
    else cost := List.nth [ 4; 10 ] (Player.utilities player - 1) * roll;
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      ("You owe " ^ Player.name player ^ " " ^ string_of_int !cost);
    try
      Player.pay !cost !current_player player;
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ("Your current balance is now: "
        ^ string_of_int (Player.balance !current_player))
    with Player.InsufficientFunds -> ())

and handle_buying cost current_player st current_square_name =
  try
    Player.bank_transaction (-cost) !current_player;
    State.acquire st current_square_name;
    print_newline ();
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("Congrats! You are the brand new owner of " ^ current_square_name
     ^ "! Your current balance is now: "
      ^ string_of_int (Player.balance !current_player))
  with Player.InsufficientFunds ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("Sorry you do not have the sufficient funds. Your current \
        balance is: "
      ^ string_of_int (Player.balance !current_player))

(** [handle_go_to_jail st] alerts the player that they are going to
    jail, and then changes the state so that their location changes to
    be in jail. *)
let handle_go_to_jail st =
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Sorry! You are now being sent straight to jail, criminal!";
  State.send_curr_jail st

(** [free_player st player] changes the state so that the player is no
    longer stuck in jail. *)
let free_player st plyr =
  plyr |> Player.clear_turns_in_jail |> Player.change_jail_status
  |> State.change_current_player st

(** [handle_doubles plyr st] keeps track of how many times a player has
    attempted to exit jail, and alerts them that they must pay if it is
    their third failed attempt. *)
let rec handle_doubles plyr st =
  if Player.turns_in_jail plyr = 3 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "It is\n   your third turn in jail. You must pay the $50 fine.\n";
    if Player.balance plyr < 50 then State.bankrupt_current_player st
    else (
      Player.bank_transaction (-50) plyr;
      free_player st plyr))
  else
    let jail_cards = Player.jail_cards plyr in
    if jail_cards > 0 then (
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("You have "
        ^ string_of_int jail_cards
        ^ ". Would you like to use one?\n");
      match read_line () with
      | "y" | "Y" -> plyr |> Player.decr_cards |> free_player st
      | "n" | "N" ->
          plyr |> Player.incr_turns_in_jail
          |> State.change_current_player st
      | _ -> handle_doubles plyr st)
    else ()

(** [handle_fine st player] charges the player if they are exiting jail
    by fine. *)
let rec handle_fine st plyr =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Would you like to pay\n   the $50 fine and be free?\n";
  match read_line () with
  | "y" | "Y" ->
      Player.bank_transaction (-50) plyr;
      plyr |> Player.clear_turns_in_jail |> Player.change_jail_status
      |> State.change_current_player st
  | "n" | "N" ->
      plyr |> Player.incr_turns_in_jail
      |> State.change_current_player st
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Check input (Y/N)!";
      handle_fine st plyr

(** [handle_turn_limit st plyr] is a helper function to alert the player
    that they must pay to exit jail, and then actually decrements their
    balance. *)
let rec handle_turn_limit st plyr =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "It is\n   your third turn in jail. You must pay the $50 fine.\n";
  if Player.balance plyr < 50 then State.bankrupt_current_player st
  else (
    Player.bank_transaction (-50) plyr;
    free_player st plyr)

(** [jail_rules st] calls the appropriate helper function for the player
    in jail's situation based on the number of times they have failed to
    exit jail and the number of get-out-of-jail-free cards they have. *)
let rec jail_rules st =
  let plyr = State.get_current_player st in
  if Player.turns_in_jail plyr = 3 then handle_turn_limit st plyr
  else
    let jail_cards = Player.jail_cards plyr in
    if jail_cards > 0 then (
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("You have "
        ^ string_of_int jail_cards
        ^ ". Would you like to use one?\n");
      match read_line () with
      | "y" | "Y" -> plyr |> Player.decr_cards |> free_player st
      | "n" | "N" ->
          plyr |> Player.incr_turns_in_jail
          |> State.change_current_player st
      | _ -> jail_rules st)
    else ();
    handle_fine st plyr

(** [handle_tax st current_square_name board] charges the player that
    landed on the square that is owned by the bank. *)
let handle_tax st current_square_name board =
  let cost = Board.cost_of_square board current_square_name in
  let current_player = ref (State.get_current_player st) in
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("You owe $" ^ string_of_int cost ^ " to the bank. Pay up now!");
  try Player.bank_transaction (-cost) !current_player
  with Player.InsufficientFunds ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Sorry you do not have the sufficient funds. You are bankrupt!";
    State.bankrupt_current_player st

let start_in_jail st plyr =
  if Player.doubles plyr = 1 then (
    ANSITerminal.print_string [ ANSITerminal.green ] "You're free!\n";
    Player.clear_turns_in_jail (Player.change_jail_status plyr)
    |> State.change_current_player st)
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Sorry, you're still in jail\n";
    jail_rules st)

let rec roll play n st =
  let plyr = State.get_current_player st in
  let board = State.get_board st in
  let first_roll = Roll.dice () in
  let second_roll = Roll.dice () in
  if first_roll = second_roll then Player.incr_doubles plyr
  else Player.clear_doubles plyr;
  let total_roll = first_roll + second_roll in
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    ("You have rolled a "
    ^ (first_roll |> string_of_int)
    ^ " and a "
    ^ (second_roll |> string_of_int)
    ^ "\n");

  let was_in_jail = State.is_in_jail st in

  if was_in_jail then start_in_jail st plyr
  else if Player.doubles plyr >= 3 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Sorry, you've rolled three doubles in a row! You are now being \
       sent to jail criminal!\n";
    Player.clear_turns_in_jail plyr |> State.change_current_player st;
    State.send_curr_jail st);

  is_in_jail_helper st total_roll board;
  roll_helper play n st first_roll second_roll was_in_jail

and is_in_jail_helper st total_roll board =
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
    | "Street" ->
        handle_property st current_square_name board total_roll
    | "Income Tax" -> handle_tax st current_square_name board
    | "Luxury Tax" -> handle_tax st current_square_name board
    | "Railroad" ->
        handle_property st current_square_name board total_roll
    | "Go to Jail" -> handle_go_to_jail st
    | "Utility" ->
        handle_property st current_square_name board total_roll
    | "Free Parking" -> ()
    | _ -> ()
  end

and roll_helper play n st first_roll second_roll was_in_jail =
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

(** [quit play n st] prompts the user for confirmation before quitting
    gameplay. If the user does not want to quit, it resumes gameplay on
    the same turn [n] and in the same state [st]. *)
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

(** [check_board play n st] prints out the next 12 squares on the board
    that follow the square that the player is currently on in state
    [st].*)
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

(** [buy_sell_buildings play n st] allows players to purchase buildings
    to put on their sets of properties. *)
let rec buy_sell_buildings play n st =
  let printer accu a = accu ^ a ^ " | " in
  let current_player = ref (State.get_current_player st) in
  let sets = Player.sets !current_player in
  let color_sets = fst (List.split sets) in
  let board = State.get_board st in

  let rec property_helper properties min_upgrade back =
    match read_line () with
    | "q" | "Q" -> play n st
    | "r" | "R" -> back ()
    | property_name ->
        prop_name_helper properties property_name min_upgrade back
  and prop_name_helper properties property_name min_upgrade back =
    if List.mem property_name properties then (
      if Board.get_current_upgrade board property_name > min_upgrade
      then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "You cannot upgrade this street. Make sure you upgrade your \
           streets evenly! Enter the name of another street, Q to \
           quit, or R to try upgrading for another set.";
        property_helper properties min_upgrade back)
      else if Board.get_current_upgrade board property_name = 5 then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          "You cannot upgrade this street. It has reached it's max \
           upgrades. Enter the name of another street, Q to quit, or R \
           to try upgrading for another set.";
        property_helper properties min_upgrade back)
      else
        let cost = Board.upgrade_cost board property_name in
        if cost > Player.balance !current_player then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "You do not have enough money to upgrade this street.";
          play n st)
        else
          ANSITerminal.print_string [ ANSITerminal.green ]
            ("Upgrading costs" ^ string_of_int cost);
        Player.bank_transaction (-cost) !current_player;
        State.upgrade st property_name;
        ANSITerminal.print_string [ ANSITerminal.green ]
          ("You have upgraded " ^ property_name
         ^ ". You now have a balance of $"
          ^ string_of_int (Player.balance !current_player)))
  and buy_sell_buildings_helper () =
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("You currently own these sets: "
      ^ List.fold_left printer "|" color_sets
      ^ ".\n\
         Enter the set you wish to buy buildings for or press Q to \
         quit!");
    match read_line () with
    | "q" | "Q" -> play n st
    | set_name ->
        if List.mem set_name color_sets then (
          let min_upgrade =
            snd (Player.get_set_by_name !current_player set_name)
          in
          let properties =
            Board.get_all_of_set (State.get_board st) set_name
          in
          ANSITerminal.print_string [ ANSITerminal.green ]
            ("You currently own these streets: "
            ^ List.fold_left printer "|" properties
            ^ ".\nEnter the street you wish to buy buildings for!");
          property_helper properties min_upgrade
            buy_sell_buildings_helper)
        else
          ANSITerminal.print_string [ ANSITerminal.red ]
            "The set name you entered does not correlate to any sets \
             you own right now. Please try again.";
        buy_sell_buildings_helper ()
  in

  if List.length sets = 0 then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "You have no sets you can buy buildings on, sorry.";
    play n st)
  else buy_sell_buildings_helper ()

(* [play n st] starts game play on turn [n] of the game, when the game
   is in state [st]. It reads the user input for what they would like to
   do on this turn. *)
let rec play n st =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Would you like to:\n";
  print_endline "----------------------------------------";
  print_endline "\t[R] - Roll";
  print_endline "\t[1] - Trade";
  print_endline "\t[2] - Mortgage/Unmortgage properties";
  print_endline "\t[3] - Buy/Sell houses/hotels";
  print_endline "\t[4] - Check current position";
  print_endline "\t[5] - Check entire board";
  print_endline "\t[6] - Check a property";
  print_endline "\t[7] - Check your balance";
  print_endline "\t[Q] - To quit";
  print_endline "----------------------------------------";
  ANSITerminal.print_string [ ANSITerminal.white ] "Option: ";
  options_reader play n st

(** [options_reader play n st] reads the user input for which option
    they chose from the game menu, and calls the corresponding helper
    functions to carry out that option. *)
and options_reader play n st =
  match read_line () with
  | "r" | "R" -> roll play n st
  | "1" -> trade_prop play n st
  | "2" -> ()
  | "3" -> buy_sell_buildings play n st
  | "4" -> check_board play n st
  | "5" ->
      Board.draw_board ();
      Unix.sleep 1;
      play n st
  | "6" -> check_property play n st
  | "7" ->
      let current = State.get_current_player st in
      ANSITerminal.print_string [ ANSITerminal.green ]
        (string_of_int (get_balance current));
      turn_printer st;
      print_newline ();
      play n st
  | "q" | "Q" -> quit play n st
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] "\nCheck input\n\n";
      print_newline ();
      turn_printer st;
      play n st

(** [main ()] prompts the user to input the number of players, and calls
    helper function [enter_player ()]. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\nWelcome to Monopoly\n";
  print_endline
    "How many people are playing today? (Please enter only a number)";
  enter_player ()

(** [enter_player ()] reads the user input of the names of each of the
    players, the piece choice of each player, and then starts the game. *)
and enter_player () =
  try
    let num_of_players = get_num_player () in
    let players =
      Array.make num_of_players (Player.create "Jacob" "Horse")
    in
    for i = 0 to num_of_players - 1 do
      print_endline
        ("Please enter the name of Player "
        ^ string_of_int (i + 1)
        ^ ".");
      let player = get_player () in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("\nPlayer " ^ string_of_int (i + 1) ^ ": " ^ player ^ "\n");
      print_endline "Choose a piece";
      let piece = get_piece () in
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("\nPlayer "
        ^ string_of_int (i + 1)
        ^ " piece: " ^ piece ^ "\n\n");
      players.(i) <- Player.create player piece
    done;
    let state =
      State.init_state players (make_board "basic_board.json")
    in
    play 0 state;
    ()
  with Failure "int_of_string" ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n Bad input! Only put letters and numbers when appropriate.";
    main ()

let () = main ()
