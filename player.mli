(** Representation of dynamic player data. *)

(** The abstract type of a player *)
type t

(** Type of player identifier *)
type player_id = string

(** Balance of a player *)
val balance : t -> int ref

(** TODO: change this type *)
val current_square : t -> string

(**TODO: check this type*)
val piece : t -> string

(**TODO: change this type*)
val properties : t -> string list

val pay : int -> t -> t -> unit
