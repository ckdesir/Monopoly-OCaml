(** The abstract type of values representing the game state. *)
type t

(** [init_state] is the initial state of the game when playing with a
    given amount of players. In this state, each player is initialized
    to have $1500 and is on "GO" *)
val init_state : Player.t array -> Board.t -> t

(** [get_player n st] is the player [n + 1] in state [st]. For example,
    [get_player 0 st] returns Player 1, etc*)
val get_player : int -> t -> Player.t

(** [change_player_at n st] mutates the player at index [n] in state
    [st]*)
val change_player_at : int -> t -> Player.t -> unit

val get_turn : t -> int

(** [get_current_player st] returns the player representing whoever's
    turn it currently is in state [st]*)
val get_current_player : t -> Player.t

(** [get_num_players st] returns the amount of players playing in state
    [st]*)
val get_num_players : t -> int

val get_board : t -> Board.t

(** [is_in_jail st] returns true if the current player is in jail. *)
val is_in_jail : t -> bool

(** [switch_turns plyr1 plyr2 b] changes the state between being
    [plyr1]'s turn or [plyr2]'s turn depending on the value of boolean
    [b]. *)
val switch_turns : t -> t

(** [move_current_player st x] alters the position of the current player
    to be [x] squares forward. If the current player passes go, the
    position wraps around and they collect $200 *)
val move_current_player : t -> int -> unit
