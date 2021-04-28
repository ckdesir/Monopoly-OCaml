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
      Some player
    else None
  in
  List.find_map get_who_helper (Array.to_list st.players)

let is_set_owned st set_name =
  let find_set color = color = set_name in
  let set_owned_helper player =
    let color_sets = fst (List.split (Player.sets player)) in
    List.exists find_set color_sets
  in
  List.exists set_owned_helper (Array.to_list st.players)

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

let completes_set st square =
  let player = get_current_player st in
  let board = st.board in
  let rec owned_of_the_set plyr sq_lst =
    (* determine the set of that square *)
    let sq_set = Board.set_of_square board square in
    (* how many properties does the player own that are of the same set
       as sq_set? *)
    let commons = [] in
    match sq_lst with
    | [] -> []
    | h :: t ->
        if Board.set_of_square board h = sq_set then h :: commons
        else owned_of_the_set plyr t
  in
  let player_owned_of_set =
    List.length (owned_of_the_set player (Player.properties player))
  in

  (* how many properties are there that are of the set that is the set
     of sq_set? *)
  let size_of_set_in_board =
    List.length (Board.get_all_of_set board square)
  in

  if player_owned_of_set = size_of_set_in_board then true else false

let acquire st square =
  let player = ref (get_current_player st) in
  let board = get_board st in
  let square_type = Board.type_of_square board square in
  player := Player.add_to_properties !player square;
  match square_type with
  | "Street" ->
      if completes_set st square then
        Player.add_to_sets (get_current_player st)
          (Board.set_of_square board square;
           (square, 0))
      else get_current_player st
  | "Railroad" -> Player.add_railroad (get_current_player st)
  | "Utility" -> Player.add_utility (get_current_player st)
  | _ -> raise (Failure "can't acquire")

let can_build_hotels st color_group =
  let helper set = fst set = color_group in
  let plyr = get_current_player st in
  let sets = Player.sets plyr in
  snd (List.find helper sets) = 4

(* match Board.nth_square_name st.board (Player.current_square
   (get_current_player st)) with | "Jail/Just Visting" ->
   Player(get_current_player st | _ -> false *)
