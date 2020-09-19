(** User interface of game. *)

(** [time] is the current time *)
let time = ref (Unix.time ())

let camel = "
3110311031103110311031103110311031103110
3110::31103110311. 031103110. 3110311031
;      ;3110311;      ';,.     ;03110311
3110.  . 311031.                 .103110
31103.  '11;.                     .03110
31103:            Congrats!        .3110
3110311.                           :3110
31103110311031;            .'.     31103
3110311031103110   :31103110310.  .31103
3110311031103110, .311031103110;   31103
31103110311031103  :311s03110311;...3110
31103110311031103..:31103110311''0',3110
3110311031103110311031103110311:31;10311\n"

(** [print_solution_board st] prints solution board from state [st] to 
    terminal *)
let print_solution_board st=
  let board = State.full_board st in 
  for x = 0 to 8 do 
    for y = 0 to 8 do 
      if y = 0 then Format.printf "\n";
      Format.printf " %d " board.(x).(y);
    done;
  done;
  print_string  "\n"

(** [print_board st curr_row curr_col isEditing isHint] prints current board 
    from state [st] to terminal *)
let rec print_board st curr_row curr_col isEditing isHint =
  (* uncomment comment below if you want to see the solution board *)
  (* print_solution_board st; *)
  let curr_board = State.current_board st in 
  let top_coord = "    0  1  2   3  4  5   6  7  8  " in
  let newBlock = "  +---------+---------+---------+" in 
  let duplicate = Board.check_duplicates (curr_row, curr_col) curr_board in
  for x = 0 to 8 do 
    if x mod 3 = 0 then print_string"\n";
    if x = 0 then print_endline top_coord;
    if x mod 3 = 0 then print_endline newBlock;
    for y = 0 to 8 do
      print_coord x y curr_row curr_col curr_board isEditing isHint duplicate
    done;
  done;
  print_string "\n";
  print_endline newBlock;
  if isEditing && duplicate != [] then manage_mistakes st else handle_command st

(** [print_norm_coord x y curr_board] prints the value in a coordinate ([x],[y]) 
    in the board that is not edited or given as hint *)
and print_norm_coord x y curr_board = 
  if (snd curr_board.(x).(y) = true)
  then ANSITerminal.(printf [green] " %d " (fst curr_board.(x).(y))) 
  else Format.printf " %d " (fst curr_board.(x).(y))

(** [print_coord x y curr_row curr_col curr_board isEditing isHint duplicate] 
      prints the value in a coordinate ([x], [y]) in the board *)
and print_coord x y curr_row curr_col curr_board isEditing isHint duplicate =
  if y = 0 && x mod 3 <> 0 then Format.printf "\n";
  if y = 0 then Format.printf "%d " x;
  if y = 0 then Format.printf "|";
  if (fst curr_board.(x).(y) = 0) then Format.printf " . " else 
    begin
      if (isHint && x = curr_row && y = curr_col)
      then ANSITerminal.(printf [cyan] " %d " (fst curr_board.(x).(y)))
      else if isEditing && List.exists (fun coord -> coord = (x, y)) duplicate 
      then ANSITerminal.(printf [red] " %d " (fst curr_board.(x).(y)))
      else print_norm_coord x y curr_board
    end;
  if y mod 3 = 2 then Format.printf "|"

(** [format_total_time] is time from start of game in minutes and seconds *)
and format_total_time () = 
  let tot_time = int_of_float(Unix.time () -. !time) in
  let time_min = (tot_time / 60) in
  print_endline ("Time: "^string_of_int(time_min)^" minute(s) and "
                 ^string_of_int(tot_time - (time_min * 60) )^" seconds")

(** [manage_mistakes st] manages when player makes a mistake in state [st] *)
and manage_mistakes st = 
  let new_state = State.deduct_mistake st in 
  match new_state with
  | State.Valid st -> 
    print_endline 
      ("You have "^(string_of_int (State.mistakes st))^" mistake(s) left."); 
    handle_command st;
  | State.Invalid s -> (
      print_endline s; 
      print_endline "You have made 3 mistakes and lost this game."; 
      format_total_time ();
      handle_game_over_command st)

(** [manage_edit_remove coord_val st] manages when player edits or removes a 
    coordinate in the board in state [st] *)
and manage_edit_remove coord_val st = 
  match coord_val with 
  | row::col::value::[] -> 
    let result = State.update_cell (row, col) value st in 
    begin 
      match result with 
      |State.Valid st -> if State.win st then begin
          print_endline (camel);
          format_total_time ();
          print_board st row col false false; end
        else print_board st row col true false;
      |State.Invalid s-> print_endline ("Invalid "^s); handle_command st
    end
  | row::col::[] -> 
    let value = fst (State.current_board st).(row).(col) in
    let result = State.remove_cell (row, col) value st in 
    begin 
      match result with 
      |State.Valid st -> print_board st row col false false
      |State.Invalid s-> print_endline ("Invalid "^s); handle_command st
    end
  | _ -> print_endline "Invalid"

(** [manage_hint st] manages when player asks for a hint in state [st] *)
and manage_hint st = 
  let result = State.give_hint st in 
  match result with 
  | State.Valid st ->
    let coord = snd (State.hint st) in
    begin
      match coord with 
      | Some (row, col) -> 
        print_endline 
          ("\nYou have "^string_of_int (fst (State.hint st))^" hint(s) left.");
        print_board st row col false true
      | None -> print_endline "no hint"; handle_command st
    end
  | State.Invalid s -> print_endline (s); handle_command st

(** [manage_undo st] manages undo command in state [st] *)
and manage_undo st = 
  let result = State.undo_moves st in 
  match result with 
  | State.Valid st -> print_board st 0 0 false false
  | State.Invalid s -> print_endline (s); handle_command st

(** [print_commands isGameOver] prints available commands based on whether 
    game is over [isGameOver] *)
and print_commands isGameOver= 
  if isGameOver then begin
    ANSITerminal.(print_string [magenta] ("\n You have the commands
   1. new game : Starts a new game
   2. restart : Restarts the current game
   3. help : Gives list of commands you can use
   4. quit : Terminates game \n")) end
  else begin 
    ANSITerminal.(print_string [magenta] ("\n You have the commands 
   1. edit (row,col,value) : Updates the value at (row, col) with the new value
   2. remove (row,col) : Removes the current value at (row, col)
   3. hint : Provides one hint
   4. undo : Undos last move
   5. time : Time passed since the game started
   6. restart : Restarts the current game
   7. new game : Starts a new game
   8. help : Gives list of commands you can use
   9. quit : Terminates game \n")) end

(** [manage_another_game st] manages playing another game in state [st] *)
and manage_another_game st = 
  time := Unix.time(); 
  print_board st 0 0 false false; 
  handle_command st

(** [handle_command_helper command st] handles valid [command] related to 
    state [st] when game is ongoing *)
and handle_command_helper command st = 
  let cmd = Commands.parse command in match cmd with 
  | Commands.Edit coord_val -> manage_edit_remove coord_val st 
  | Commands.Remove coord_val -> manage_edit_remove coord_val st 
  | Commands.Hint -> let hint = fst (State.hint st) in 
    if hint = 0 
    then (print_endline "You have no more hint left"; handle_command st)
    else begin 
      print_endline ("You have "^string_of_int hint^" hint(s) left.");
      print_endline "Are you sure you want to use a hint? (y/n)";
      handle_option_cmd st false 
    end
  | Commands.New -> handle_level true
  | Commands.Restart -> 
    let new_state = State.restart_game st in manage_another_game new_state
  | Commands.Help -> print_commands false; handle_command st;
  | Commands.Quit -> print_endline "Are you sure you want to quit? (y/n) ";
    handle_option_cmd st true
  | Commands.Time -> format_total_time (); handle_command st
  | Commands.Undo -> manage_undo st

(** [handle_command st] handles command related to state [st] when 
    game is ongoing *)
and handle_command st = 
  print_string "\n> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> 
    try handle_command_helper command st
    with 
    | Commands.Empty -> print_endline "Command cannot be empty"; 
      handle_command st
    | Commands.Malformed -> print_endline "Command is malformed"; 
      handle_command st

(** [handle_quit_cmd] handles quit command *)
and handle_option_cmd st isQuit = 
  match read_line () with
  | exception End_of_file -> ()
  | cmd -> match cmd with
    | "y" -> if isQuit 
      then (print_endline "Goodbye."; exit 0)
      else manage_hint st
    | "n" -> handle_command st
    | _ -> print_endline "Enter (y/n) "; handle_option_cmd st isQuit

(** [handle_game_over_command st] handles commands related to state [st] when 
    game is over *)
and handle_game_over_command st = 
  print_string "\n > ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> 
    try let cmd = Commands.parse command in
      match cmd with 
      | Commands.New -> handle_level true
      | Commands.Restart -> 
        let new_state = State.restart_game st in manage_another_game new_state
      | Commands.Help -> print_commands true; handle_game_over_command st
      | Commands.Quit -> handle_option_cmd st true
      | _ -> (print_endline "Game has ended. This command is invalid."; 
              handle_game_over_command st)
    with 
    | Commands.Empty -> print_endline "Command cannot be empty"; 
      handle_game_over_command st
    | Commands.Malformed -> print_endline "Command is malformed";
      handle_game_over_command st

(**[handle_level_helper level isNew] handles the level [level] *)
and handle_level_helper level isNew= 
  try let lvl = Commands.parse_lvl level in
    let board_lvl = 
      match lvl with 
      | Commands.Easy -> Board.BEasy
      | Commands.Medium -> Board.BMedium
      | Commands.Hard -> Board.BHard in 
    let start_state = State.init_state board_lvl in 
    if not isNew then print_commands false;
    print_board start_state 0 0 false false;
    match read_line () with
    | exception End_of_file -> ()
    | _ -> handle_command start_state
  with
  | Commands.Empty -> print_endline "Level cannot be empty."; 
    handle_level isNew
  | Commands.Malformed -> 
    print_endline "Level is malformed."; handle_level isNew

(** [handle_level] manages level of difficulty based on player input *)
and handle_level isNew =
  print_string 
    "\nEnter the level you want to play: easy, medium, or hard \n> ";
  match read_line () with
  | exception End_of_file -> ()
  | level -> handle_level_helper level isNew

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to Sudoku.\n");
  handle_level false

(* Execute the game engine. *)
let () = main ()