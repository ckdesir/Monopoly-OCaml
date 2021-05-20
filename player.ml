type player_id = string

exception InsufficientFunds

type player = {
  name : player_id;
  piece : string;
  current_square : int;
  mutable balance : int;
  properties : Board.square_name list;
  is_bankrupt : bool;
  get_out_of_jail_cards : int;
  mutable doubles : int;
  is_in_jail : bool;
  turns_in_jail : int;
  sets : (string * int) list;
  railroads : int;
  utilities : int;
}

type t = player

let name t = t.name

let piece t = t.piece

let current_square t = t.current_square

let balance t = t.balance

let properties t = t.properties

let doubles t = t.doubles

let sets t = t.sets

let railroads t = t.railroads

let utilities t = t.utilities

let bankrupt t = t.is_bankrupt

let make_bankrupt p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = 0;
    properties = [];
    is_bankrupt = true;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = [];
    railroads = 0;
    utilities = 0;
  }

let is_in_jail t = t.is_in_jail

let turns_in_jail t = t.turns_in_jail

let change_jail_status p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = not p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let incr_turns_in_jail p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail + 1;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let clear_turns_in_jail p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = 0;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let set_position p x =
  {
    name = p.name;
    piece = p.piece;
    current_square = x;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let add_to_properties p property =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = property :: p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let add_to_sets p set =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = set :: p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let replace_a_set p set_name new_set =
  let rec replace_set_helper = function
    | [] -> []
    | h :: t ->
        if fst h = set_name then new_set :: t
        else h :: replace_set_helper t
  in
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = replace_set_helper p.sets;
    railroads = p.railroads;
    utilities = p.utilities;
  }

let add_railroad p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads + 1;
    utilities = p.utilities;
  }

let remove_railroad p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads - 1;
    utilities = p.utilities;
  }

let add_utility p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities + 1;
  }

let remove_utility p =
  {
    name = p.name;
    piece = p.piece;
    current_square = p.current_square;
    balance = p.balance;
    properties = p.properties;
    is_bankrupt = p.is_bankrupt;
    get_out_of_jail_cards = p.get_out_of_jail_cards;
    doubles = p.doubles;
    is_in_jail = p.is_in_jail;
    turns_in_jail = p.turns_in_jail;
    sets = p.sets;
    railroads = p.railroads;
    utilities = p.utilities - 1;
  }

let get_set_by_name t set_name =
  let find_helper set = fst set = set_name in
  List.find find_helper t.sets

let create n p =
  {
    name = n;
    piece = p;
    current_square = 0 (*"Go"*);
    balance = 1500;
    properties = [];
    is_bankrupt = false;
    get_out_of_jail_cards = 0;
    doubles = 0;
    is_in_jail = false;
    turns_in_jail = 0;
    sets = [];
    railroads = 0;
    utilities = 0;
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

let owns player square = List.mem square player.properties

let incr_doubles t =
  t.doubles <- t.doubles + 1;
  ()

let clear_doubles t =
  t.doubles <- 0;
  ()

let incr_cards player =
  {
    name = player.name;
    piece = player.piece;
    current_square = player.current_square;
    balance = player.balance;
    properties = player.properties;
    is_bankrupt = player.is_bankrupt;
    get_out_of_jail_cards = player.get_out_of_jail_cards + 1;
    doubles = player.doubles;
    is_in_jail = player.is_in_jail;
    turns_in_jail = player.turns_in_jail;
    sets = player.sets;
    railroads = player.railroads;
    utilities = player.utilities;
  }

let decr_cards player =
  {
    name = player.name;
    piece = player.piece;
    current_square = player.current_square;
    balance = player.balance;
    properties = player.properties;
    is_bankrupt = player.is_bankrupt;
    get_out_of_jail_cards = player.get_out_of_jail_cards - 1;
    doubles = player.doubles;
    is_in_jail = player.is_in_jail;
    turns_in_jail = player.turns_in_jail;
    sets = player.sets;
    railroads = player.railroads;
    utilities = player.utilities;
  }

let remove_elts elt_lst lst =
  List.filter (fun x -> not (List.mem x elt_lst)) lst

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
    doubles = player.doubles;
    is_in_jail = player.is_in_jail;
    turns_in_jail = player.turns_in_jail;
    sets = player.sets;
    railroads = player.railroads;
    utilities = player.utilities;
  }

let get_props player prop_list =
  let new_lst = List.merge Stdlib.compare player.properties prop_list in
  {
    name = player.name;
    piece = player.piece;
    current_square = player.current_square;
    balance = player.balance;
    properties = new_lst;
    is_bankrupt = player.is_bankrupt;
    get_out_of_jail_cards = player.get_out_of_jail_cards;
    doubles = player.doubles;
    is_in_jail = player.is_in_jail;
    turns_in_jail = player.turns_in_jail;
    sets = player.sets;
    railroads = player.railroads;
    utilities = player.utilities;
  }

let trade
    p1
    p2
    (p1_props : Board.square_name list)
    (p2_props : Board.square_name list) : player * player =
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
      doubles = giver.doubles;
      is_in_jail = giver.is_in_jail;
      turns_in_jail = giver.turns_in_jail;
      sets = giver.sets;
      railroads = giver.railroads;
      utilities = giver.utilities;
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
      doubles = recip.doubles;
      is_in_jail = recip.is_in_jail;
      turns_in_jail = giver.turns_in_jail;
      sets = recip.sets;
      railroads = recip.railroads;
      utilities = recip.utilities;
    }
  in
  (giver, recip)
