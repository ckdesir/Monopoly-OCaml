type t = {
  player1 : Player.player_id;
  player2 : Player.player_id;
  player_1s_turn : bool;
}

let init_state plyr1 plyr2 =
  {
    player1 = "Player 1" (*Player.name plyr1*);
    player2 = "Player 2" (*Player.name plyr2*);
    player_1s_turn = true;
  }

let get_player1 st = st.player1

let get_player2 st = st.player2

let switch_turns plyr1 plyr2 b =
  { player1 = plyr1; player2 = plyr2; player_1s_turn = b }

(*later the boolean will be a call/turn number to have more players*)

(* let move plyr = try *)
(* let switch_turns plyr1 plyr2 st = let new_player_name = Player.name
   in let new_state = { name = } *)
(* keep track of turn number in state *)
