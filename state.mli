(** The abstract type of values representing the game state. *)
type t

(** [init_state plyr1 plyr2] is the initial state of the game when
    playing with players [plyr1] and [plyr2]. In this state, the players
    are respectively named "Player 1" and "Player 2", and it is Player
    1's turn. *)
val init_state : string -> string -> t

(** [get_player1 st] is the identifier of [player1] in state [st]. *)
val get_player1 : t -> Player.player_id

(** [get_player2 st] is the identifier of [player2] in state [st]. *)
val get_player2 : t -> Player.player_id

(** [switch_turns plyr1 plyr2 b] changes the state between being
    [plyr1]'s turn or [plyr2]'s turn depending on the value of boolean
    [b]. *)
val switch_turns : string -> string -> bool -> t
