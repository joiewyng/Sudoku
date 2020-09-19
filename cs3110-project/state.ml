open Board

(** The type representing the state of a sudoku game. *)
type state = 
  {full_board: (Board.init_board);
   current_board: Board.current_board;
   hint: int * Board.coordinate option;
   mistakes: int;
   moves: Commands.command list}

type t = state

type result = Valid of t | Invalid of string

exception Break

let init_state lvl = 
  let full_board = Board.create_init_board () in 
  {full_board = full_board;
   current_board = Board.create_curr_board full_board lvl;
   hint = (3, None);
   mistakes = 3;
   moves = []}

let full_board st = 
  st.full_board

let current_board st = 
  st.current_board

let hint st = 
  st.hint

let mistakes st = 
  st.mistakes

let update_cell coord value st = 
  try let new_board = Board.update coord value st.current_board in 
    Valid {full_board = st.full_board;
           current_board = new_board; 
           hint = st.hint;
           mistakes = st.mistakes;
           moves = (Commands.Edit [fst coord; snd coord; value])::st.moves}
  with 
  | Board.InvalidCoordinate coord -> Invalid "coordinate"
  | Board.InvalidValue value -> Invalid "value"

let remove_cell coord value st = 
  try let new_board = Board.remove coord st.current_board in 
    let moves = if value = 0 then st.moves 
      else (Commands.Remove [fst coord; snd coord; value])::st.moves in
    Valid {full_board = st.full_board;
           current_board = new_board;
           hint = st.hint;
           mistakes = st.mistakes;
           moves = moves}
  with Board.InvalidCoordinate coord -> Invalid "coordinate"

let give_hint st = 
  if (fst st.hint) > 0
  then let hint_result = Board.get_hint st.current_board st.full_board in 
    let new_board = fst hint_result in 
    let coord = snd hint_result in
    Valid {full_board = st.full_board;
           current_board = new_board;
           hint = ((fst st.hint-1), Some coord);
           mistakes = st.mistakes;
           moves = st.moves}
  else Invalid "You have used all your hints"

let deduct_mistake st = 
  if st.mistakes-1 = 0
  then Invalid "Game Over"
  else Valid {full_board = st.full_board;
              current_board = st.current_board;
              hint = st.hint;
              mistakes = st.mistakes-1;
              moves = st.moves}

let win st =
  try
    for i = 0 to 8 do 
      for j = 0 to 8 do 
        if (fst st.current_board.(i).(j)) != st.full_board.(i).(j) 
        then raise Break else ();
      done;
    done;
    true
  with Break -> false

(** [undo_updated row col value st tl isEdit] is the result with the board 
    updated to the new state *)
let undo_updated coord_val st tl isEdit= 
  match coord_val with
  | row::col::value::[] -> 
    let new_board = 
      if isEdit 
      then Board.remove (row, col) st.current_board
      else Board.update (row, col) value st.current_board in
    Valid {full_board = st.full_board;
           current_board = new_board;
           hint = st.hint;
           mistakes = st.mistakes;
           moves = tl}
  | _ -> Valid st

let undo_moves st = 
  match st.moves with 
  |hd::tl -> 
    begin
      match hd with 
      | Commands.Edit coord_val -> undo_updated coord_val st tl true
      | Commands.Remove coord_val -> undo_updated coord_val st tl false
      | _ -> Valid st
    end
  | [] -> Invalid "No more moves to undo"

let restart_game st = 
  {full_board = st.full_board;
   current_board = Board.unfilled_curr_board st.current_board;
   hint = (3, None);
   mistakes = 3;
   moves = []}
