type player_id = string

type player = {
  name : player_id;
  piece : string;
  current_square : string;
  mutable balance : int ref;
  properties : string list;
}

type t = player

let balance t = !(t.balance)

let piece t = t.piece

let current_square t = t.current_square

let properties t = t.properties

let is_in_jail t =
  match t.current_square with "jail" -> true | _ -> false

let pay amt giver recip =
  giver.balance := !(giver.balance) - amt;
  recip.balance := !(recip.balance) + amt;
  ()

let move (player : player) (amt : int) (curr : string) : player =
  failwith "Unimplemented"
