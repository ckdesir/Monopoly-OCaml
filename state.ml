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

(* let get_who_owns st property_name = (** Options Some whoever or None
   *) let rec get_who_owns_helper plyr =

   (* let rec get_who_owns_helper st = let current = Array.get
   st.players (st.current_player) in match Player.properties current
   with | [] -> None | h :: t -> begin match get_who_owns_helper t with
   | None -> Player.create "" "" | Some x -> current end *) try (* *)
   Array.iter get_who_owns_helper st.players *)

let switch_turns s =
  {
    players = s.players;
    num_players = s.num_players;
    current_player = (s.current_player + 1) mod s.num_players;
    board = s.board;
  }

let move_current_player st roll =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Moving " ^ string_of_int roll ^ " squares");

  let plyr = get_current_player st in
  let new_position = (Player.current_square plyr + roll) mod 40 in
  let new_player = Player.set_position plyr new_position in
  if new_position < Player.current_square plyr then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Pass Go, collect $200!";

    Player.pass_go new_player)
  else ();
  change_current_player st new_player

let is_in_jail st = Player.is_in_jail (get_current_player st)

(* match Board.nth_square_name st.board (Player.current_square
   (get_current_player st)) with | "Jail/Just Visting" ->
   Player(get_current_player st | _ -> false *)
