(** The abstract type of values representing the game state. *)
type t

(*TODO: CHANGE THIS*)

(** [init_state plyr1 plyr2] is the initial state of the game when
    playing with players [plyr1] and [plyr2]. In this state, the players
    are respectively named "Player 1" and "Player 2", and it is Player
    1's turn. *)
val init_state : Player.t array -> Board.t -> t

(** [get_player1 st] is the identifier of [player1] in state [st]. *)
val get_player : int -> t -> Player.t

val get_turn : t -> int

val get_current_player : t -> Player.t

val get_num_players : t -> int

val get_board : t -> Board.t

(** [switch_turns plyr1 plyr2 b] changes the state between being
    [plyr1]'s turn or [plyr2]'s turn depending on the value of boolean
    [b]. *)
val switch_turns : t -> t
