type square_type = string

type square_name = string

exception TypeMismatch of square_type

exception UnknownSquare of square_name

exception UnknownType of square_type

exception InvalidSquare of (square_type * square_name)

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
  chance_cards : chance_card list;
  community_chest_cards : community_chest list;
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
  for i = n to (n + 12 - 1) do
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
    let valid_types =
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
    in
    let find valid_type = square.stype = valid_type in
    try List.find find valid_types
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

let get_chance_card board =
  match board.chance_cards with
  | [] -> raise (Failure "No Chance Cards left")
  | h :: t ->
      ( h,
        {
          squares = board.squares;
          chance_cards = t @ [ h ];
          community_chest_cards = board.community_chest_cards;
        } )

let get_community_chest_card board =
  match board.community_chest_cards with
  | [] -> raise (Failure "No Community Chests left")
  | h :: t ->
      ( h,
        {
          squares = board.squares;
          chance_cards = board.chance_cards;
          community_chest_cards = t @ [ h ];
        } )
