(** Representation of static board data.

    This module represents the data stored in board files, including
    the squares themselves and chance and community cards. It handles loading 
    of that data from JSON as well as querying the data. *)

(** The abstract type of values representing boards. *)
type t

(** The type of the square. *)
type square_type = string

(** The name of the square. *)
type square_name = string

(** Raised when an operation is done on a type that doesn't match.
    For example, obtaining the mortgage of a "Community Chest" type
*)
exception TypeMismatch of square_type

(** Raised when an unknown square is encountered. *)
exception UnknownSquare of square_name

(** [from_json j] is the board that [j] represents. Requires: [j] is
    a valid JSON Monopoly board representation. *)
val from_json : Yojson.Basic.t -> t

(** [cost_of_square b s] is the cost of square [s] on board [b].
    Cost is associated with buying square / landing on square if applicable.
    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_square].
    The following square_types are valid for cost_of_square: 
    [["Street", "Income Tax", "Luxury Tax", "Railroad", "Utility"]] *)
val cost_of_square : t -> square_name -> int

(** [mortgage_of_square b s] is the mortgage of square [s] on board [b].
    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [mortgage_of_square].
    The following square_types are valid for mortgage_of_square: 
    [["Street", "Railroad", "Utility"]] *)
val mortgage_of_square : t -> square_name -> int

(** [position_of_square b s] is the position of square [s] on board [b].
    Raises [UnknownSquare s] if [s] is not a square in [b].
    All square_types should have a valid position on the board *)
val position_of_square : t -> square_name -> int

(** [type_of_square b s] is the type of square [s] on board [b].
    Raises [UnknownSquare s] if [s] is not a square in [b].
    All squares should have a square_type.
    The following square_types are valid for an instance of board:
    [["Go", "Jail/Just Visitng", "Chance", "Community Chest", 
      "Street", "Income Tax", "Luxury Tax", "Railroad", "Utility", 
      "Free Parking", "Go To Jail"]] *)
val type_of_square : t -> square_name -> square_type

(** [contains b s] returns true if square [s] is on board [b]. *)
val contains : t -> square_name -> bool

(** [upgrade_cost b s] is the upgrade cost of square [s] on board [b].
    Upgrade cost is the amount of money required to pay for a house/hotel.
    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [upgrade_cost].
    The following square_types are valid for upgrade_cost: 
    [["Street"]] *)
val upgrade_cost : t -> square_name -> int

(** [cost_of_tier_0_rent b s] is the cost of tier 0 rent of
    square [s] on board [b].

    Tier 0 rent describes, for square_type [["Street"]] the rent that must
    be paid given that there are no houses/hotels on that lot.
    If a player owns ALL the lots of any set for square_type [["Street"]]
    Tier 0 rent is doubled (up to whoever maintains the state of the game).
    
    Tier 0 rent describes, for square_type [["Railroad"]], the rent that must
    be paid given that a player owns no other railroad on the board.

    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_tier_0_rent].
    The following square_types are valid for cost_of_tier_0_rent: 
    [["Street", "Railroad"]] *)
val cost_of_tier_0_rent : t -> square_name -> int

(** [cost_of_tier_1_rent b s] is the cost of tier 1 rent of
    square [s] on board [b].

    Tier 1 rent describes, for square_type [["Street"]] the rent that must
    be paid given that there is one house on that lot.
    
    Tier 1 rent describes, for square_type [["Railroad"]], the rent that must
    be paid given that a player owns one other railroad on the board.

    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_tier_1_rent].
    The following square_types are valid for cost_of_tier_1_rent: 
    [["Street", "Railroad"]] *)
val cost_of_tier_1_rent : t -> square_name -> int

(** [cost_of_tier_2_rent b s] is the cost of tier 2 rent of
    square [s] on board [b].

    Tier 2 rent describes, for square_type [["Street"]] the rent that must
    be paid given that there are two houses on that lot.
    
    Tier 2 rent describes, for square_type [["Railroad"]], the rent that must
    be paid given that a player owns two other railroads on the board.

    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_tier_2_rent].
    The following square_types are valid for cost_of_tier_2_rent: 
    [["Street", "Railroad"]] *)
val cost_of_tier_2_rent : t -> square_name -> int

(** [cost_of_tier_3_rent b s] is the cost of tier 3 rent of
    square [s] on board [b].

    Tier 3 rent describes, for square_type [["Street"]] the rent that must
    be paid given that there are three houses on that lot.
    
    Tier 3 rent describes, for square_type [["Railroad"]], the rent that must
    be paid given that a player owns the other three railroads on the board.

    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_tier_3_rent].
    The following square_types are valid for cost_of_tier_3_rent: 
    [["Street", "Railroad"]] *)
val cost_of_tier_3_rent : t -> square_name -> int

(** [cost_of_tier_4_rent b s] is the cost of tier 4 rent of
    square [s] on board [b].

    Tier 4 rent describes, for square_type [["Street"]] the rent that must
    be paid given that there are four houses on that lot.
    
    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_tier_4_rent].
    The following square_types are valid for cost_of_tier_4_rent: 
    [["Street"]] *)
val cost_of_tier_4_rent : t -> square_name -> int

(** [cost_of_tier_5_rent b s] is the cost of tier 5 rent of
    square [s] on board [b].

    Tier 5 rent describes, for square_type [["Street"]] the rent that must
    be paid given that there is a hotel on that lot.
    
    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [cost_of_tier_5_rent].
    The following square_types are valid for cost_of_tier_5_rent: 
    [["Street"]] *)
val cost_of_tier_5_rent : t -> square_name -> int

(* val chance_card_list : t -> ???

val community_card_list : t -> ??? *)
