(** Representation of dynamic player data. *)

(** The abstract type of a player *)
type t

(** Type of player identifier *)
type player_id = string

(** Raised when a player tries to perform an action but has insufficient
    balance*)
exception InsufficientFunds

(**[name t] is the string representing the name of the person playing as
   the player [t]*)
val name : t -> player_id

(** [piece t] is the token that the player [t] is represented by on a
    physical monopoly board. For example, ["thimble"], ["top hat"] would
    be valid values of [piece t] on a traditional monopoly board*)
val piece : t -> string

(** [current_square t] is the square position that player [t] occupies. *)
val current_square : t -> int

(** [balance t] is the value of the mutable balance of player [t]*)
val balance : t -> int

(**[properties t] is the list of properties that are owned by the player
   represented by [t] *)
val properties : t -> Board.square_name list

(**[bankrupt t] represents whether or not the player [t] is bankrupt,
   and thus determines if they are still in the game*)
val bankrupt : t -> bool

(* (** [is_in_jail t] is whether or not the player represented by [t] is
   currently in jail. *) val is_in_jail : t -> bool *)

(** [create n p] instantiates a player of type [t] with name [n], piece
    [p], current_square of "GO", balance of 1500, properties [],
    is_bankrupt false, and get_out_of_jail_cards of 0. *)
val create : player_id -> string -> t

(** [pay int t p] alters two player's mutable balances to perform the
    physical action of paying in Monopoly. For example, if [t] had $600
    and [p] had $400 then [pay 100 t p] would result in [balance t]
    being [500] and [balance p] being [500]. Raises [InsufficentFunds]
    if [t] lacks balance to perform [pay]*)
val pay : int -> t -> t -> unit

(** [pay_bank amt t] alters player [t]'s balance by an amount [amt]. For
    example, when passing Go a player would see the equivalent of
    [bank_transaction 200 t]. Note: [amt] may be positive or negative,
    corresponding to receiving or paying money to the bank respectively.
    Raises [InsufficientFunds] if the player lacks funds.*)
val bank_transaction : int -> t -> unit

(** [pass_go t] adds 200 to player [t]'s balance, equivalent to
    [bank_transaction 200 t]*)
val pass_go : t -> unit

(** [jail_cards t] represents the amount of get-out-of-jail free cards
    that player [t] is currently holding. If [jail_cards t] is 0, then
    the player will be sent to jail if prompted*)
val jail_cards : t -> int

(* (** [send_to_jail t] returns an identical player as [t] excdpt their
   current square is located at "Jail", representing the action of
   moving a piece to jail in Monopoly. *) val send_to_jail : t -> t *)

(** [acquire t s] returns an identical player as [t] except the square s
    is added [t]'s list of properties*)
val acquire : t -> Board.square_name -> t

(**[trade p1 p2 p1_props p2_props] is a pair of players [p1] and [p2]
   with the various exchanged properties, mirroring if players traded
   squares in real life. Note: cannot trade get_out_of_jail cards. *)
val trade :
  t -> t -> Board.square_name list -> Board.square_name list -> t * t

(** [owns t s] returns the boolean of whether or not player [t] owns the
    square [s]*)
val owns : t -> Board.square_name -> bool

(** [incr cards p] returns the a player identical to [p] with one more
    get-out-of-jail-free card*)
val incr_cards : t -> t

(**[trade_cards p1 p2 amt] is a pair of players [p1] and [p2] that have
   exchanged [amt] get-out-of-jail cards, with [p1] giving [amt] to [p2]*)
val trade_cards : t -> t -> int -> t * t

(** [doubles t] returns the amount of consecutive doubles [t] has rolled*)
val doubles : t -> int
