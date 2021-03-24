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

type t = {
  squares : square list;
  (* chance : chance list;
  community : community list; *)
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

let from_json json = {
  squares = json |> member "squares" |> to_list |> List.map squares_of_board;
  (* chance = json |> member "chance" |> to_list |> List.map chance_cards;
  community = json |> member "community" |> to_list |> List.map community_chest_cards; *)
}