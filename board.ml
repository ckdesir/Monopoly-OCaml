type square_type = string

type square_name = string

exception TypeMismatch of square_type

exception UnknownSquare of square_name

exception UnknownType of square_type

exception InvalidSquare of (square_type * square_name)

exception MaxUpgradeReached

let universal_valid_types =
  [
    "Go";
    "Jail/Just Visiting";
    "Chance";
    "Community Chest";
    "Street";
    "Income Tax";
    "Luxury Tax";
    "Railroad";
    "Utility";
    "Free Parking";
    "Go to Jail";
  ]

type rent = {
  tier : int option;
  cost : int option;
}

type square = {
  name : square_name;
  stype : square_type;
  position : int;
  cost : int option;
  upgrade_cost : int option;
  set : string option;
  rent_tiers : rent list option;
  mortgage : int option;
  mutable current_upgrade : int option;
  max_upgrade : int option;
  player_here : bool;
}

type chance_card = {
  title : string;
  action : string;
  name : square_name option;
  position : int option;
  stype : square_type option;
  rent_multiplier : int option;
  subaction : string option;
  amount : int option;
  count : int option;
  house : int option;
  hotels : int option;
}

type community_chest = {
  title : string;
  action : string;
  subaction : string option;
  position : int option;
  amount : int option;
  house : int option;
  hotels : int option;
}

type t = {
  squares : square list;
  mutable chance_cards : chance_card list;
  mutable community_chest_cards : community_chest list;
}

open Yojson.Basic.Util

let rent_tiers_of_board json =
  let helper j =
    {
      tier = j |> member "tier" |> to_int_option;
      cost = j |> member "cost" |> to_int_option;
    }
  in
  List.map helper (to_list json)

let squares_of_board json =
  {
    name = json |> member "name" |> to_string;
    stype = json |> member "type" |> to_string;
    position = json |> member "position" |> to_int;
    cost = json |> member "cost" |> to_int_option;
    upgrade_cost = json |> member "upgrade cost" |> to_int_option;
    set = json |> member "set" |> to_string_option;
    mortgage = json |> member "mortgage" |> to_int_option;
    rent_tiers = json |> member "rent" |> to_option rent_tiers_of_board;
    current_upgrade = json |> member "current upgrade" |> to_int_option;
    max_upgrade = json |> member "max upgrade" |> to_int_option;
    player_here = false;
  }

let chance_cards_of_board json =
  {
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

let community_chest_cards_of_board json =
  {
    title = json |> member "title" |> to_string;
    action = json |> member "action" |> to_string;
    subaction = json |> member "subaction" |> to_string_option;
    position = json |> member "position" |> to_int_option;
    amount = json |> member "amount" |> to_int_option;
    house = json |> member "house" |> to_int_option;
    hotels = json |> member "hotels" |> to_int_option;
  }

let from_json json =
  {
    squares =
      json |> member "squares" |> to_list |> List.map squares_of_board;
    chance_cards =
      json |> member "chance" |> to_list
      |> List.map chance_cards_of_board;
    community_chest_cards =
      json |> member "community" |> to_list
      |> List.map community_chest_cards_of_board;
  }

let find_square board s =
  let find (potential_square : square) = potential_square.name = s in
  List.find find board.squares

let contains board s =
  let square_finder (square : square) = square.name = s in
  List.exists square_finder board.squares

let nth_square_name board pos = (List.nth board.squares pos).name

let next_twelve board n =
  let next_twelve_string = ref "| " in
  for i = n to n + 11 do
    next_twelve_string :=
      !next_twelve_string ^ nth_square_name board (i mod 40) ^ " | "
  done;
  !next_twelve_string

let position_of_square board s =
  let rec accumulate_positions (squares : square list) =
    match squares with
    | [] -> []
    | h :: t ->
        if h.name = s then h.position :: accumulate_positions t
        else accumulate_positions t
  in
  let positions = accumulate_positions board.squares in
  if List.length positions = 0 then raise (UnknownSquare s)
  else positions

let type_of_square board s =
  try
    let square = find_square board s in
    let find valid_type = square.stype = valid_type in
    try List.find find universal_valid_types
    with Not_found -> raise (UnknownType square.stype)
  with
  | Not_found -> raise (UnknownSquare s)
  | UnknownType exn -> raise (UnknownType exn)

let square_operation_helper board s valid_types matcher =
  try
    let type_square = type_of_square board s in
    let find valid_type = type_square = valid_type in
    try
      let valid_type = List.find find valid_types in
      let square = find_square board s in
      matcher square (Failure valid_type)
    with
    | Not_found -> raise (TypeMismatch type_square)
    | Failure exn -> raise (InvalidSquare (type_square, s))
  with
  | UnknownSquare exn -> raise (UnknownSquare exn)
  | UnknownType exn -> raise (UnknownType exn)
  | TypeMismatch exn -> raise (TypeMismatch exn)
  | InvalidSquare exn -> raise (InvalidSquare exn)

let cost_of_square board s =
  let matcher square excp =
    match square.cost with Some cost -> cost | None -> raise excp
  in
  let valid_types =
    [ "Street"; "Income Tax"; "Luxury Tax"; "Railroad"; "Utility" ]
  in
  square_operation_helper board s valid_types matcher

let mortgage_of_square board s =
  let matcher square excp =
    match square.mortgage with
    | Some mortgage -> mortgage
    | None -> raise excp
  in
  square_operation_helper board s
    [ "Street"; "Railroad"; "Utility" ]
    matcher

let set_of_square board s =
  let matcher square excp =
    match square.set with Some set -> set | None -> raise excp
  in
  square_operation_helper board s
    [ "Street"; "Railroad"; "Utility" ]
    matcher

let get_current_upgrade board s =
  let matcher square excp =
    match square.current_upgrade with
    | Some upgrade -> upgrade
    | None -> raise excp
  in
  let valid_types = [ "Street"; "Railroad" ] in
  square_operation_helper board s valid_types matcher

let upgrade_helper board s num condition_excp condition =
  let matcher square excp =
    match square.current_upgrade with
    | Some upgrade ->
        if condition square then raise condition_excp
        else square.current_upgrade <- Some (upgrade + num)
    | None -> raise excp
  in
  let valid_types = [ "Street"; "Railroad" ] in
  square_operation_helper board s valid_types matcher

let incr_upgrade board s =
  let condition square = square.current_upgrade = square.max_upgrade in
  upgrade_helper board s 1 MaxUpgradeReached condition

(* let decr_upgrade board s = let condition square =
   square.current_upgrade = Some 0 in upgrade_helper board s (-1)
   MinUpgradeReached condition *)

let get_all_of_set board set_name =
  let rec helper accum = function
    | [] -> accum
    | h :: t -> (
        match h.set with
        | None -> helper accum t
        | Some set ->
            if set = set_name then helper (h.name :: accum) t
            else helper accum t )
  in
  helper [] board.squares

let upgrade_cost board s =
  let matcher square excp =
    match square.upgrade_cost with
    | Some upgrade_cost -> upgrade_cost
    | None -> raise excp
  in
  square_operation_helper board s [ "Street" ] matcher

let sort_rent_tiers rent_tiers =
  let compare_rent_tiers x y =
    Option.compare Int.compare x.tier y.tier
  in
  List.sort compare_rent_tiers rent_tiers

let cost_of_tier_0_rent board s =
  let matcher square excp =
    match square.rent_tiers with
    | Some rent_tiers ->
        let sorted_tiers = sort_rent_tiers rent_tiers in
        let tier_0 = List.hd sorted_tiers in
        if Option.is_none tier_0.tier || Option.is_none tier_0.cost then
          raise excp
        else tier_0.cost
    | None -> raise excp
  in
  match
    square_operation_helper board s [ "Street"; "Railroad" ] matcher
  with
  | Some cost -> cost
  | None -> raise (InvalidSquare (type_of_square board s, s))

let cost_of_tier_1_rent board s =
  let matcher square excp =
    match square.rent_tiers with
    | Some rent_tiers ->
        let sorted_tiers = sort_rent_tiers rent_tiers in
        let tier_1 = List.nth sorted_tiers 1 in
        if Option.is_none tier_1.tier || Option.is_none tier_1.cost then
          raise excp
        else tier_1.cost
    | None -> raise excp
  in
  match
    square_operation_helper board s [ "Street"; "Railroad" ] matcher
  with
  | Some cost -> cost
  | None -> raise (InvalidSquare (type_of_square board s, s))

let cost_of_tier_2_rent board s =
  let matcher square excp =
    match square.rent_tiers with
    | Some rent_tiers ->
        let sorted_tiers = sort_rent_tiers rent_tiers in
        let tier_2 = List.nth sorted_tiers 2 in
        if Option.is_none tier_2.tier || Option.is_none tier_2.cost then
          raise excp
        else tier_2.cost
    | None -> raise excp
  in
  match
    square_operation_helper board s [ "Street"; "Railroad" ] matcher
  with
  | Some cost -> cost
  | None -> raise (InvalidSquare (type_of_square board s, s))

let cost_of_tier_3_rent board s =
  let matcher square excp =
    match square.rent_tiers with
    | Some rent_tiers ->
        let sorted_tiers = sort_rent_tiers rent_tiers in
        let tier_3 = List.nth sorted_tiers 3 in
        if Option.is_none tier_3.tier || Option.is_none tier_3.cost then
          raise excp
        else tier_3.cost
    | None -> raise excp
  in
  match
    square_operation_helper board s [ "Street"; "Railroad" ] matcher
  with
  | Some cost -> cost
  | None -> raise (InvalidSquare (type_of_square board s, s))

let cost_of_tier_4_rent board s =
  let matcher square excp =
    match square.rent_tiers with
    | Some rent_tiers ->
        let sorted_tiers = sort_rent_tiers rent_tiers in
        let tier_4 = List.nth sorted_tiers 4 in
        if Option.is_none tier_4.tier || Option.is_none tier_4.cost then
          raise excp
        else tier_4.cost
    | None -> raise excp
  in
  match square_operation_helper board s [ "Street" ] matcher with
  | Some cost -> cost
  | None -> raise (InvalidSquare (type_of_square board s, s))

let cost_of_tier_5_rent board s =
  let matcher square excp =
    match square.rent_tiers with
    | Some rent_tiers ->
        let sorted_tiers = sort_rent_tiers rent_tiers in
        let tier_5 = List.nth sorted_tiers 5 in
        if Option.is_none tier_5.tier || Option.is_none tier_5.cost then
          raise excp
        else tier_5.cost
    | None -> raise excp
  in
  match square_operation_helper board s [ "Street" ] matcher with
  | Some cost -> cost
  | None -> raise (InvalidSquare (type_of_square board s, s))

let cost_of_rent board s =
  let square =
    try find_square board s with Not_found -> raise (UnknownSquare s)
  in
  match square.current_upgrade with
  | Some upgrade -> (
      match upgrade with
      | 0 -> cost_of_tier_0_rent board s
      | 1 -> cost_of_tier_1_rent board s
      | 2 -> cost_of_tier_2_rent board s
      | 3 -> cost_of_tier_3_rent board s
      | 4 -> cost_of_tier_4_rent board s
      | 5 -> cost_of_tier_5_rent board s
      | _ -> raise (InvalidSquare (square.stype, s)) )
  | None ->
      if square.stype = "Street" || square.stype = "Railroad" then
        raise (InvalidSquare (square.stype, s))
      else
        let find square_type = square_type = square.stype in
        if List.exists find universal_valid_types then
          raise (TypeMismatch square.stype)
        else raise (UnknownType square.stype)

let get_chance_card board =
  match board.chance_cards with
  | [] -> raise (Failure "No Chance Cards left")
  | h :: t ->
      board.chance_cards <- t @ [ h ];
      h

let get_community_chest_card board =
  match board.community_chest_cards with
  | [] -> raise (Failure "No Community Chests left")
  | h :: t ->
      board.community_chest_cards <- t @ [ h ];
      h

let draw_board () =
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -------------------------------------------------------------------------------------------------------------------------\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|   Free   ";
  ANSITerminal.print_string [ ANSITerminal.red ] "| Kentucky |";
  ANSITerminal.print_string [ ANSITerminal.white ] "  Chance  ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|  Indiana ";
  ANSITerminal.print_string [ ANSITerminal.red ] "| Illinois |";
  ANSITerminal.print_string [ ANSITerminal.white ] "    B&O   ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "| Atlantic ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "| Ventnor  |";
  ANSITerminal.print_string [ ANSITerminal.white ] "   Water  ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|  Marvin  |";
  ANSITerminal.print_string [ ANSITerminal.white ] "   Go To  |\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "| Parking  ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|    Ave   |";
  ANSITerminal.print_string [ ANSITerminal.white ] "          ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|    Ave   ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|    Ave   |";
  ANSITerminal.print_string [ ANSITerminal.white ] "    RR    ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|    Ave   ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|    Ave   |";
  ANSITerminal.print_string [ ANSITerminal.white ] "   Works  ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|  Gardens |";
  ANSITerminal.print_string [ ANSITerminal.white ] "   Jail   |\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "|          ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -------------------------------------------------------------------------------------------------------------------------\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "| New York |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "| Pacific  |\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|    Ave   |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|    Ave   |\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|Tennessee |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|  North   |\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|    Ave   |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "| Caro Ave |\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|Community |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|Community |\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|   Chest  |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|   Chest  |\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|          |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "| St James |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|   Penn   |\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|   Place  |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|   Ave    |\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "|          |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.green ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "|   Penn   \
     |                                                                                                  \
     |   Short  |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "|    RR    \
     |                                                                                                  \
     |   Line   |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "|          \
     |                                                                                                  \
     |          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "| Virginia \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|  Chance  |\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|   Ave    \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|          |\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|          \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.white ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|  States  \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "|   Park   |\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|   Ave    \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "|   Place  |\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|          \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "| Electric \
     |                                                                                                  \
     |  Luxury  |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "| Company  \
     |                                                                                                  \
     |   Tax    |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "|          \
     |                                                                                                  \
     |          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -----------                                                                                                  \
     ------------\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|St \
     Charles|                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "|Boardwalk |\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|  Place   \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "|          |\n";
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "|          \
     |                                                                                                  ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "|          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -------------------------------------------------------------------------------------------------------------------------\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|Visit|Jail";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "|   Conn   | Vermont  |";
  ANSITerminal.print_string [ ANSITerminal.white ] "  Chance  ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "| Oriental |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " Reading  |  Income  |  Baltic  |Community |    Med   |   Go     |\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|     |    ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "|    Ave   |    Ave   |          |    Ave   |";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "    RR    |    Tax   |    Ave   |  Chest   |    Ave   |          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ] "|     |    ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "|          |          |          |          |          ";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "|          |          |          |          |          |\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    " \
     -------------------------------------------------------------------------------------------------------------------------\n"
