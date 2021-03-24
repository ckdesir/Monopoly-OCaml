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
    "Street", "Income Tax", "Luxury Tax", "Railroad", "Utility" *)
val cost_of_square : Yojson.Basic.t -> square_name -> int

(** [mortgage_of_square b s] is the mortgage of square [s] on board [b].
    Raises [UnknownSquare s] if [s] is not a square in [b].
    Raises [TypeMismatch] if the square_type of [s] is an invalid operation on 
    [mortgage_of_square].
    The following square_types are valid for mortgage_of_square: 
    "Street", "Railroad", "Utility" *)
val mortgage_of_square : Yojson.Basic.t -> square_name -> int

val position_of_square : Yojson.Basic.t -> square_name -> int

val type_of_square : Yojson.Basic.t -> square_name -> square_type

val cost_of_house : Yojson.Basic.t -> square_name -> int

val cost_of_hotel : Yojson.Basic.t -> square_name -> int

val cost_of_basic_rent : Yojson.Basic.t -> square_name -> int

val cost_of_tier_1_rent : Yojson.Basic.t -> square_name -> int

val cost_of_tier_2_rent : Yojson.Basic.t -> square_name -> int

val cost_of_tier_3_rent : Yojson.Basic.t -> square_name -> int

val cost_of_tier_4_rent : Yojson.Basic.t -> square_name -> int

val cost_of_final_tier_rent : Yojson.Basic.t -> square_name -> int

(* val tier_1_definition : Yojson.Basic.t -> square_name -> string

val tier_2_definition : Yojson.Basic.t -> square_name -> string

val tier_3_definition : Yojson.Basic.t -> square_name -> string

val tier_4_definition : Yojson.Basic.t -> square_name -> string

val final_tier_definition : Yojson.Basic.t -> square_name -> string *)