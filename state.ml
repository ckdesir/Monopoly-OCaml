type t = {
  (* players : Player.t list; *)
  players : Player.t array;
  num_players : int;
  current_player : int;
  board : Board.t;
}

let init_state player_list board =
  {
    players = player_list;
    num_players = Array.length player_list;
    current_player = 0;
    board;
  }

let change_player_at n st new_player = st.players.(n) <- new_player

let change_current_player st new_player =
  change_player_at st.current_player st new_player

let get_player n st = st.players.(n)

let get_current_player st = get_player st.current_player st

let get_turn st = st.current_player

let get_num_players st = st.num_players

let get_board st = st.board

let get_who_owns st property_name =
  let find_piece square_name = square_name = property_name in
  let get_who_helper player =
    if List.exists find_piece (Player.properties player) then
      Some (Player.name player)
    else None
  in
  List.find_map get_who_helper (Array.to_list st.players)

let switch_turns s =
  {
    players = s.players;
    num_players = s.num_players;
    current_player = (s.current_player + 1) mod s.num_players;
    board = s.board;
  }

let move_current_player st roll =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Moving " ^ string_of_int roll ^ " squares\n");

  let plyr = get_current_player st in
  let new_position = (Player.current_square plyr + roll) mod 40 in
  let new_player = Player.set_position plyr new_position in
  if new_position < Player.current_square plyr then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Pass Go, collect $200!\n";

    Player.pass_go new_player)
  else ();
  change_current_player st new_player

let send_curr_jail st =
  let jail_num =
    List.hd
      (Board.position_of_square (get_board st) "Jail/Just Visiting")
  in
  let plyr =
    Player.set_position (get_current_player st) jail_num
    |> Player.change_jail_status
  in
  change_current_player st plyr;
  ANSITerminal.print_string [ ANSITerminal.red ]
    (Player.name plyr ^ " is now in jail.")

let is_in_jail st = Player.is_in_jail (get_current_player st)

(* match Board.nth_square_name st.board (Player.current_square
   (get_current_player st)) with | "Jail/Just Visting" ->
   Player(get_current_player st | _ -> false *)
