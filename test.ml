open OUnit2
open Board

let basic_board = from_json (Yojson.Basic.from_file "basic_board.json")
let board_test =
  [ 
    
  ]

let suite =
  "test suite for Final project"
  >::: List.flatten [ board_test; ]

let _ = run_test_tt_main suite
