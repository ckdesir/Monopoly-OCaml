type t = {
  (* players : Player.t list; *)
  players : Player.t list;
  num_players : int;
  current_player : int;
  board : Board.t;
}

let init_state player_list board =
  {
    players = player_list;
    num_players = List.length player_list;
    current_player = 0;
    board;
  }

let get_player n st = List.nth st.players n

let get_current_player st = get_player st.current_player st

let get_turn st = st.current_player

let get_num_players st = st.num_players

let switch_turns s =
  {
    players = s.players;
    num_players = s.num_players;
    current_player = (s.current_player + 1) mod s.num_players;
    board = s.board;
  }

(*later the boolean will be a call/turn number to have more players*)

(* let move plyr = try *)
(* let switch_turns plyr1 plyr2 st = let new_player_name = Player.name
   in let new_state = { name = } *)
(* keep track of turn number in state *)
