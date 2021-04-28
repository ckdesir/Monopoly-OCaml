(** The abstract type of values representing the game state. *)
type t

(** [init_state pl b] is the initial state of the game when playing with
    a given amount of players. In this state, each player is initialized
    to have $1500 and is on "GO" *)
val init_state : Player.t array -> Board.t -> t

(** [get_player n st] is the player [n + 1] in state [st]. For example,
    [get_player 0 st] returns Player 1, etc*)
val get_player : int -> t -> Player.t

(** [change_player_at n st] mutates the player at index [n] in state
    [st]*)
val change_player_at : int -> t -> Player.t -> unit

(** [get_turn st] returns the integer representing the player whose turn
    it currently is in state [st]. *)
val get_turn : t -> int

(** [change_current_player st new_plyr] changes [st] so that [new_plyr]
    becomes the player whose turn it currently is. *)
val change_current_player : t -> Player.t -> unit

(** [get_current_player st] returns the player representing whoever's
    turn it currently is in state [st]*)
val get_current_player : t -> Player.t

(** [get_num_players st] returns the amount of players playing in state
    [st]*)
val get_num_players : t -> int

(** [get_board b] returns the board that is being used for gameplay. *)
val get_board : t -> Board.t

(** [get_who_owns st prop] returns the player who owns [prop] in [st] if
    it is owned. Otherwise, it returns [None] *)
val get_who_owns : t -> Board.square_name -> Player.t option

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

(** [send_curr_jail st] returns an identical position except the current
    player is now standing on the "jail" square and is now in jail.*)
val send_curr_jail : t -> unit
