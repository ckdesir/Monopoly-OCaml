type player_id = string

exception InsufficientFunds

type player = {
  name : player_id;
  piece : string;
  current_square : Board.square_name;
  mutable balance : int;
  properties : Board.square_name list;
  is_bankrupt : bool;
  get_out_of_jail_cards : int;
}

type t = player

let name t = t.name

let piece t = t.piece

let current_square t = t.current_square

let balance t = t.balance

let properties t = t.properties

let bankrupt (player : player) : bool = player.is_bankrupt

let is_in_jail t =
  match t.current_square with
  | "Jail/Just Visiting" -> true
  | _ -> false

let create n p =
  {
    name = n;
    piece = p;
    current_square = "Go";
    balance = 0;
    properties = [];
    is_bankrupt = false;
    get_out_of_jail_cards = 0;
  }

let pay amt giver recip =
  if amt > giver.balance then raise InsufficientFunds
  else giver.balance <- giver.balance - amt;
  recip.balance <- recip.balance + amt;
  ()

let bank_transaction amt player =
  let x = player.balance + amt in
  if x < 0 then raise InsufficientFunds else player.balance <- x;
  ()

let pass_go player = bank_transaction 200 player

let jail_cards (player : player) : int = player.get_out_of_jail_cards

let send_to_jail (player : player) : player =
  match jail_cards player with
  | 0 ->
      {
        name = player.name;
        piece = player.piece;
        current_square = "jail";
        balance = player.balance;
        properties = player.properties;
        is_bankrupt = player.is_bankrupt;
        get_out_of_jail_cards = 0;
      }
  | x ->
      {
        name = player.name;
        piece = player.piece;
        current_square = player.current_square;
        balance = player.balance;
        properties = player.properties;
        is_bankrupt = player.is_bankrupt;
        get_out_of_jail_cards = x - 1;
      }

let owns player square = List.mem square player.properties

let trade
    p1
    p2
    (p1_props : Board.square_name list)
    (p2_props : Board.square_name list) : player * player =
  let remove_elts elt_lst lst =
    List.filter (fun x -> not (List.mem x elt_lst)) lst
  in

  let remove_props player prop_lst =
    let new_lst = remove_elts prop_lst player.properties in
    {
      name = player.name;
      piece = player.piece;
      current_square = player.current_square;
      balance = player.balance;
      properties = new_lst;
      is_bankrupt = player.is_bankrupt;
      get_out_of_jail_cards = player.get_out_of_jail_cards;
    }
  in

  let get_props player prop_list =
    let new_lst =
      List.merge Stdlib.compare player.properties prop_list
    in
    {
      name = player.name;
      piece = player.piece;
      current_square = player.current_square;
      balance = player.balance;
      properties = new_lst;
      is_bankrupt = player.is_bankrupt;
      get_out_of_jail_cards = player.get_out_of_jail_cards;
    }
  in

  let p1 = remove_props p1 p1_props in
  let p1 = get_props p1 p2_props in
  let p2 = remove_props p2 p2_props in
  let p2 = get_props p2 p1_props in

  (p1, p2)

let trade_cards giver recip amt =
  let giver =
    {
      name = giver.name;
      piece = giver.piece;
      current_square = giver.current_square;
      balance = giver.balance;
      properties = giver.properties;
      is_bankrupt = giver.is_bankrupt;
      get_out_of_jail_cards = giver.get_out_of_jail_cards - amt;
    }
  in
  let recip =
    {
      name = recip.name;
      piece = recip.piece;
      current_square = recip.current_square;
      balance = recip.balance;
      properties = recip.properties;
      is_bankrupt = recip.is_bankrupt;
      get_out_of_jail_cards = recip.get_out_of_jail_cards + amt;
    }
  in
  (giver, recip)
