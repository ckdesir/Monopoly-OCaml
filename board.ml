type square_type = string

type square_name = string

exception TypeMismatch of square_type

exception UnknownSquare of square_name

type rent = {
  tier: int;
  cost: int;
}

type square = {
  name: square_name;
  stype: square_type;
  position: int;
  cost: int option;
  upgrade_cost: int option;
  set: string option;
  rent_tiers: rent list option;
  mortgage: int option;
}

type chance_card = {
  title: string;
  action: string;
  name: square_name option;
  position: int option;
  stype: square_type option;
  rent_multiplier: int option;
  subaction: string option;
  amount: int option;
  count: int option;
  house: int option;
  hotels: int option;
}

type community_chest = {
  title: string;
  action: string;
  subaction: string option;
  amount: int option;
  house: int option;
  hotels: int option;
}

type t = {
  squares : square list;
  chance_cards : chance_card list;
  community_chest_cards : community_chest list;
}

open Yojson.Basic.Util

let rent_tiers_of_board json = 
  let helper j = {
    tier = j |> member "tier" |> to_int;
    cost = j |> member "cost" |> to_int;
  } in
  List.map helper (to_list json)

let squares_of_board json = {
  name = json |> member "name" |> to_string;
  stype = json |> member "type" |> to_string;
  position = json |> member "position" |> to_int;
  cost = json |> member "cost" |> to_int_option;
  upgrade_cost = json |> member "upgrade cost" |> to_int_option;
  set = json |> member "set" |> to_string_option;
  mortgage = json |> member "mortgage" |> to_int_option;
  rent_tiers = json |> member "rent" |> to_option rent_tiers_of_board;
}

let chance_cards_of_board json = {
  title = json |> member "title" |> to_string;
  action = json |> member "action" |> to_string;
  name = json |> member "action" |> to_string_option;
  position = json |> member "position" |> to_int_option;
  stype = json |> member "type" |> to_string_option;
  rent_multiplier = json |> member "rentmultiplier" |> to_int_option;
  subaction = json |> member "subaction" |> to_string_option;
  amount = json |> member "amount" |> to_int_option;
  count = json |> member "count" |> to_int_option;
  house = json |> member "house" |> to_int_option;
  hotels = json |> member "hotels" |> to_int_option;
}

let community_chest_cards_of_board json = {
  title = json |> member "title" |> to_string;
  action = json |> member "action" |> to_string;
  subaction = json |> member "subaction" |> to_string_option;
  amount = json |> member "amount" |> to_int_option;
  house = json |> member "house" |> to_int_option;
  hotels = json |> member "hotels" |> to_int_option;
}
let from_json json = {
  squares = 
    json |> member "squares" |> to_list |> List.map squares_of_board;
  chance_cards = 
    json |> member "chance" |> to_list |> List.map chance_cards_of_board;
  community_chest_cards = json |> member "community" |> to_list |> 
    List.map community_chest_cards_of_board;
}