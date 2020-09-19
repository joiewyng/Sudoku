(** Test Plan: 
    We used OUnit to test what we implemented in the modules board and commands.
    The test cases were developed using black box testing where we created tests
    based on the specifications.

    We were not able to write OUnit test for module state because the board is 
    randomly created. Instead, we tested state by playing the game. Module main 
    was also tested by playing the game.

    This approach demonstrates correctness of the system because the functions 
    in modules board and commands are based on a single board that we can 
    change. Thus, testing based on specifications ensures that the functions 
    operate as intended. However, for modules state and main, we must test 
    based on the dynamic aspect of the game. Thus, we tested them by playing 
    the game. 
*)

open OUnit2
open Board
open Commands
open State

(** [test_full] is an example of a full sudoku board *)
let test_full = 
  [|
    [|9; 3; 8; 4; 1; 5; 6; 2; 7 |]; 
    [|5; 7; 1; 3; 2; 6; 9; 4; 8 |]; 
    [|2; 6; 4; 7; 8; 9; 1; 5; 3 |]; 
    [|4; 9; 5; 6; 3; 1; 7; 8; 2 |]; 
    [|1; 2; 7; 5; 9; 8; 3; 6; 4 |]; 
    [|6; 8; 3; 2; 4; 7; 5; 9; 1 |]; 
    [|3; 1; 9; 8; 6; 4; 2; 7; 5 |]; 
    [|8; 5; 2; 9; 7; 3; 4; 1; 6 |]; 
    [|7; 4; 6; 1; 5; 2; 8; 3; 9 |]; 
  |]

(** [unplayed_board] is an example of a unplayed sudoku board*)
let unplayed_board = [|
  [|(0, true); (3, false); (8, false); (0, true); (1, false); (5, false); 
    (6, false); (0, true); (7, false)|]; 
  [|(5, false); (7, false); (0, true); (3, false); (2, false); (6, false); 
    (0, true); (4, false); (8, false)|]; 
  [|(2, false); (0, true); (4, false); (7, false); (8, false); (9, false); 
    (0, true); (5, false); (3, false) |]; 
  [|(0, true); (9, false); (0, true); (6, false); (0, true); (1, false); 
    (7, false); (8, false); (2, false) |]; 
  [|(1, false); (2, false); (7, false); (5, false); (9, false); (8, false); 
    (3, false); (6, false); (4, false) |]; 
  [|(0, true); (0, true); (3, false); (0, true); (4, false); (7, false); 
    (5, false); (9, false); (1, false) |]; 
  [|(3, false); (1, false); (0, true); (8, false); (6, false); (4, false); 
    (0, true); (7, false); (5, false) |]; 
  [|(8, false); (0, true); (2, false); (9, false); (7, false); (3, false); 
    (4, false); (1, false); (6, false) |]; 
  [|(0, true); (4, false); (6, false); (1, false); (0, true); (0, true); 
    (8, false); (3, false); (9, false) |]; |]

(** [played_board] is an example of a played sudoku board*)
let played_board = [|
  [|(7, true); (3, false); (8, false); (0, true); (1, false); (5, false); 
    (6, false); (0, true); (7, false)|]; 
  [|(5, false); (7, false); (0, true); (3, false); (2, false); (6, false); 
    (0, true); (4, false); (8, false)|]; 
  [|(2, false); (0, true); (4, false); (7, false); (8, false); (9, false); 
    (0, true); (5, false); (3, false) |]; 
  [|(0, true); (9, false); (5, true); (6, false); (0, true); (1, false); 
    (7, false); (8, false); (2, false) |]; 
  [|(1, false); (2, false); (7, false); (5, false); (9, false); (8, false); 
    (3, false); (6, false); (4, false) |]; 
  [|(8, true); (8, true); (3, false); (0, true); (4, false); (7, false); 
    (5, false); (9, false); (1, false) |]; 
  [|(3, false); (1, false); (0, true); (8, false); (6, false); (4, false); 
    (0, true); (7, false); (5, false) |]; 
  [|(8, false); (0, true); (2, false); (9, false); (7, false); (3, false); 
    (4, false); (1, false); (6, false) |]; 
  [|(0, true); (4, false); (6, false); (1, false); (0, true); (0, true); 
    (8, false); (3, false); (9, false) |]; |]

(** [make_update_test name coord curr] constructs an 
    OUnit test named [name] that asserts the quality of [value]
    with [fst (update coord value curr).(fst coord).(snd coord))]. *)
let make_update_test
    (name : string)
    (coord : Board.coordinate)
    (value : int)
    (curr : Board.current_board) : test  = 
  name >:: (fun _ -> 
      assert_equal value 
        (fst (update coord value curr).(fst coord).(snd coord)))

(** [make_update_exception_test name coord curr] constructs an OUnit test 
    named [name] that asserts the InvalidCoordinate [coord] exception raised 
    with [update coord value curr]. *)
let make_update_exception_test 
    (name : string) 
    (coord : Board.coordinate)
    (value : int)
    (curr : Board.current_board) : test = 
  name >:: (fun _ -> 
      assert_raises (InvalidCoordinate coord) (fun()->update coord value curr);
    )

(** [make_update_val_exception_test name coord curr] constructs an OUnit test 
    named [name] that asserts the InvalidValue [value] exception raised 
    with [update coord value curr]. *)
let make_update_val_exception_test 
    (name : string) 
    (coord : Board.coordinate)
    (value : int)
    (curr : Board.current_board) : test = 
  name >:: (fun _ -> 
      assert_raises (InvalidValue value) (fun() -> update coord value curr);
    )

(** [make_remove_exception_test name coord curr] constructs an OUnit test 
    named [name] that asserts the InvalidCoordinate [coord] exception raised 
    with [remove coord curr]. *)
let make_remove_exception_test 
    (name : string) 
    (coord : Board.coordinate)
    (curr : Board.current_board) : test = 
  name >:: (fun _ -> 
      assert_raises (InvalidCoordinate coord) (fun() -> remove coord curr);
    )

(** [make_remove_test name coord curr] constructs an 
    OUnit test named [name] that asserts the integer [0]
    with [fst (update coord value curr).(fst coord).(snd coord))]. *)
let make_remove_test
    (name : string)
    (coord : Board.coordinate)
    (curr : Board.current_board) : test  = 
  name >:: (fun _ -> 
      assert_equal 0 
        (fst (remove coord curr).(fst coord).(snd coord)))

(** [make_unfilled_curr_board_test name played expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output]
    with [unfilled_curr_board played]. *)
let make_unfilled_curr_board_test
    (name : string)
    (played : Board.current_board)
    (expected_output : Board.current_board) : test  = 
  name >:: (fun _ -> 
      assert_equal expected_output (unfilled_curr_board played))

(** [make_check_dup_test name coord played expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output]
    with [check_duplicates coord played]. *)
let make_check_dup_test 
    (name : string)
    (coord : Board.coordinate)
    (played: Board.current_board)
    (expected_output : Board.coordinate list) : test  = 
  name >:: (fun _ ->
      assert_equal expected_output (check_duplicates coord played))

(** [make_command_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [parse input]. *)
let make_command_test
    (name : string) 
    (input: string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse input))

let curr_board = Board.create_curr_board test_full BEasy

let update_tests = [
  make_update_exception_test "update negative x coord" (-1, 1) 1 curr_board;
  make_update_exception_test "update > 8 x coord" (9, 5) 2 curr_board;
  make_update_exception_test "update negative y coord" (0, -1) 3 curr_board;
  make_update_exception_test "update > 8 y coord" (8, 100) 4 curr_board;
  make_update_exception_test "update non-hidden coord" (0, 2) 5 unplayed_board;
  make_update_val_exception_test "zero value" (1, 1) 0 unplayed_board;
  make_update_val_exception_test "negative value" (1, 1) (-1) unplayed_board;
  make_update_val_exception_test ">9 value" (1, 1) 10 unplayed_board;
  make_update_test "update" (8,0) 6 unplayed_board;
]

let remove_tests = [
  make_remove_exception_test "rmv negative x coord" (-1, 1) curr_board;
  make_remove_exception_test "rmv > 8 x coord" (9, 5) curr_board;
  make_remove_exception_test "rmv negative y coord" (0, -1) curr_board;
  make_remove_exception_test "rmv > 8 y coord" (8, 100) curr_board;
  make_remove_exception_test "rmv non-hidden coord" (0, 2) unplayed_board;
  make_remove_test "remove" (0, 0) unplayed_board;
]

let more_board_tests = [
  make_unfilled_curr_board_test "test unfilled curr board" played_board 
    unplayed_board;
  make_check_dup_test "check has dup" (0, 0) played_board [(0,8); (1,1)];
  make_check_dup_test "check no dup" (0, 1) played_board [];
]

let commands_tests = 
  [
    make_command_test "edit (1,2,3)" "edit (1,2,3)" (Edit [1; 2; 3]);
    make_command_test "    edit (1;2;3)    " 
      "    edit (1,2,3)   " (Edit [1; 2; 3]);
    make_command_test "edit (1 , 2 , 3)" "edit (1,2,3)" (Edit [1; 2; 3]);
    make_command_test "    edit ( 1,2,3 )    " 
      "    edit (1,2,3)   " (Edit [1; 2; 3]);
    "command - invalid 2 arg edit"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "edit (1,2)"));
    "command - invalid comma edit"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "edit (1;2;3)"));

    make_command_test "remove (1,2)" "remove (1,2)" (Remove [1; 2]);
    make_command_test "    remove (1,2)    " 
      "    remove (1,2)   " (Remove [1; 2]);
    make_command_test "remove (1 , 2 )" "remove (1,2)" (Remove [1; 2]);
    make_command_test "    remove ( 1,2)    " 
      "    remove (1,2)   " (Remove [1; 2]);
    "command - invalid 3 arg remove"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "remove (1,2,3)"));
    "command - invalid comma remove"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "remove (1;2)"));


    make_command_test "restart" "restart" (Restart);
    "command - invalid restart"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "restart now"));
    make_command_test "new game" "new game" (New);
    make_command_test "new      game" "new      game" (New);
    "command - invalid newgame"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "newgame"));
    "command - invalid new game"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "new game now"));

    make_command_test "undo" "undo" (Undo);
    "command - invalid undo"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "undo now"));
    make_command_test "quit" "quit" (Quit);
    "command - invalid quit"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "quit now"));
    make_command_test "hint" "hint" (Hint);
    "command - invalid hint"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "hint now"));
    make_command_test "help" "help" (Help);
    "command - invalid help"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "help now"));
    "command - invalid edit"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "edit"));
    "command - invalid command"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "Edit"));
    "command - invalid edit"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "remove"));
    "command - invalid command"    >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "Remove"));
    "command - empty input"    >:: 
    (fun _ -> assert_raises (Empty) (fun () -> parse ""));
    "command - only spaces input"    >:: 
    (fun _ -> assert_raises (Empty) (fun () -> parse "     "));
  ]

let suite =
  "test suite for Soduku"  >::: List.flatten [
    commands_tests;
    remove_tests;
    update_tests;
    more_board_tests;
  ]

let _ = run_test_tt_main suite