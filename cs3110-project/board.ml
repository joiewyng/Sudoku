type init_board = int array array
type current_board = (int*bool) array array
type coordinate = int*int

type level =
  | BEasy
  | BMedium
  | BHard

exception InvalidCoordinate of coordinate
exception InvalidValue of int
exception Break

(** [board_zeros] creates a 2d 9x9 array filled with 0s *)
let board_zeros = Random.self_init (); Array.make_matrix 9 9 0

(** [make_copy] board is a an 2d array containing the elements of [board] *)
let make_copy board = 
  let new_board = Array.make_matrix 9 9 0 in 
  for i = 0 to 8 do 
    for j = 0 to 8 do 
      new_board.(i).(j) <- board.(i).(j)
    done;
  done;
  new_board

(** [sorted_values] is an array with integers 1-9 *)
let sorted_values = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]

(** [shuffled_values] is an array containing elements of sorted_values 
    randomly shuffled *)
let shuffled_values sorted_values = 
  let copy = Array.copy sorted_values in 
  for i = 8 downto 1 do 
    let j = Random.int (i+1) in
    let temp = copy.(i) in
    copy.(i) <- copy.(j);
    copy.(j) <- temp
  done;
  copy

(** [is_incomplete_board board] is true if [board] contains zero value(s). 
    False otherwise.*)
let is_incomplete_board board = 
  Array.fold_left (fun acc elt -> (Array.exists (fun x -> x = 0) elt) || acc) 
    false board

(** [all_squares board] is an array containing values in [board] grouped into 
    9 3x3 suqares *)
let all_squares board = 
  let square = Array.copy board_zeros in
  for i = 0 to 8 do
    square.(i) <- Array.concat
        [(Array.sub board.(0+i-(i mod 3)) ((i mod 3)*3) 3); 
         (Array.sub board.(1+i-(i mod 3)) ((i mod 3)*3) 3); 
         (Array.sub board.(2+i-(i mod 3)) ((i mod 3)*3) 3)];
  done;
  square

(** [get_sqaure row col board] is a 1x9 array containing the values in the 
    square corresponding to the [row] and [col]. *)
let get_square row col board =
  let square = all_squares board in
  if row<3 then 
    if col <3 then square.(0) 
    else if col <6 then square.(1)
    else square.(2)
  else if row<6 then 
    if col <3 then square.(3) 
    else if col <6 then square.(4)
    else square.(5)
  else 
    (if col <3 then square.(6) 
     else if col <6 then square.(7)
     else square.(8))

(** [col_vals board col] is an array containing values in column [col] of 
    board [board]*)
let col_vals board col=
  let colvals = Array.make 9 0 in
  for r = 0 to 8 do
    colvals.(r) <- board.(r).(col);
  done;
  colvals

(** [invalid_location value row col board] is true if row, column, and square 
    does contain [value]. False otherwise.  *)
let invalid_location value row col board = 
  (* check if the value is in the row *)
  Array.exists (fun x -> x = value) board.(row) 
  (* check if the value is in the col *)
  || Array.exists (fun x -> x = value) (col_vals board col)  
  (* check if the value is in the square *)
  || Array.exists (fun x -> x = value) (get_square row col board)

(** [get_empty_coord board coord] is Some (row, col) where the value of [board] 
    at (row, col) is 0. None if there are no 0 values in [board] *)
let get_empty_coord board = 
  let coord = ref None in
  try 
    for box = 0 to 80 do 
      let row = box/9 in
      let col = box mod 9 in
      if board.(row).(col) = 0 
      then (coord := Some (row, col); raise Break) 
      else ()
    done; 
    None
  with Break -> !coord

(** [backtrack board] is true is [board] is complete. 

    Backtrack Algorithm inspired by: 
    https://www.geeksforgeeks.org/sudoku-backtracking-7/ and 
    https://www.101computing.net/sudoku-generator-algorithm/ *)
let rec backtrack board = 
  let coord = get_empty_coord board in 
  match coord with 
  | None -> true
  | Some (row, col) -> 
    let value_arr = shuffled_values sorted_values in 
    try    
      for i = 0 to 8 do
        let value = value_arr.(i) in
        if not (invalid_location value row col board) then
          begin
            board.(row).(col) <- value;
            if backtrack board then raise Break else ();
            board.(row).(col) <- 0
          end
        else ();
      done;
      false
    with Break -> true

let counter = ref 0

let empty_counter = ref 0

(** [check_counter] is unit if counter is not more than 1.
    Raises: [Break] if [counter] is more than 1. *)
let check_counter () = 
  if !counter > 1 then raise Break else ()

(** [backtrack_solve board] is true if there is more than
    one unique solution to the [board] or if the [board] is full.

    Backtrack Algorithm inspired by: 
    https://www.geeksforgeeks.org/sudoku-backtracking-7/ and 
    https://www.101computing.net/sudoku-generator-algorithm/ 
*)
let rec backtrack_solve board = 
  let coord = get_empty_coord board in 
  match coord with 
  | None -> true
  | Some (row, col) -> 
    let value_arr = sorted_values in 
    try 
      for i = 0 to 8 do
        let value = value_arr.(i) in
        if not (invalid_location value row col board) then
          begin
            board.(row).(col) <- value;
            if backtrack_solve board
            then (counter := !counter + 1; check_counter ())
            else ();
            board.(row).(col) <- 0;
          end
        else ();
      done; 
      false
    with Break -> true

(** [create_init_board] initalizes a 9x9 board *)
let create_init_board () = 
  let board = make_copy board_zeros in 
  if backtrack board then board else board

(** [random_coor board] is the coordinates (row, col) of a random zero 
    value in [board] if [is_zero] is true. 
    Random non-zero value if [is_zero] is false *)
let rec random_coor board is_zero =
  let row = Random.int 9 in
  let col = Random.int 9 in
  let zero = if is_zero then board.(row).(col)=0 else board.(row).(col)<>0 in 
  if zero
  then (row, col)
  else random_coor board is_zero

(** [curr_board_loop board empty_in_level] is board with [empty_in_level] 
    number of 0s *)
let rec curr_board_loop board empty_in_level = 
  if !empty_counter < empty_in_level then 
    let coord = random_coor board false in 
    let row = fst coord in 
    let col = snd coord in 
    let value = board.(row).(col) in 
    board.(row).(col) <- 0; 
    empty_counter := !empty_counter + 1;
    let board_copy = make_copy board in
    counter := 0; 
    if backtrack_solve board_copy 
    then 
      begin
        board.(row).(col) <- value; 
        empty_counter := !empty_counter -1;
        curr_board_loop board empty_in_level 
      end 
    else curr_board_loop board empty_in_level
  else board

(** [create_curr_board_helper] is a integer array with some 0 values *)
let create_curr_board_helper full_board lvl = 
  empty_counter := 0;
  let empty_in_level = 
    match lvl with
    | BEasy -> 39 + Random.int 4
    | BMedium -> 42 + Random.int 4
    | BHard -> 49 + Random.int 5 in
  curr_board_loop (make_copy full_board) empty_in_level

(** [create_curr_board lvl] is a (int * bool) array with hidden values 
    (0, true *)
let create_curr_board full_board lvl = 
  let board = create_curr_board_helper full_board lvl in 
  let acc = Array.make_matrix 9 9 (0, true) in 
  for i = 0 to 8 do 
    for j = 0 to 8 do
      let is_zero = board.(i).(j) = 0 in 
      acc.(i).(j) <- (board.(i).(j), is_zero)
    done;
  done;
  acc

(** [revert_curr_board curr] is an array containing the integer values from 
    [curr]*)
let revert_curr_board curr = 
  let acc = Array.make_matrix 9 9 0 in 
  for i = 0 to 8 do 
    for j = 0 to 8 do
      acc.(i).(j) <- fst curr.(i).(j)
    done;
  done;
  acc

(** [valid_coord row col curr] is true if box at coordinates [row], [col] in 
    board [curr] is hidden. False otherwise.

    Raises: InvalidCoordinate [row], [col] if [row] or [col] is not 
    in range 0-8 *)
let valid_coord row col curr = 
  if row > 8 || row < 0 || col > 8 || col < 0 
  then raise (InvalidCoordinate (row, col))
  else match curr.(row).(col) with
    | (_, hidden) -> hidden

let update coord value curr =
  if (value < 1 || value > 9) 
  then raise (InvalidValue value)
  else match coord with
    | (row, col) -> 
      if (valid_coord row col curr) = false
      then raise (InvalidCoordinate coord)
      else curr.(row).(col) <- (value, true);
      curr

let remove coord curr =
  match coord with
  | (row, col) ->
    if (valid_coord row col curr) = false
    then raise (InvalidCoordinate coord)
    else curr.(row).(col) <- (0, true);
    curr

(** [find_index2 arr value] is list of indices in [arr] with value [value] *)
let find_index2 arr value = 
  let rec loop arr value index acc = 
    match index with
    | i when i < Array.length arr -> 
      if arr.(index) = value 
      then loop arr value (index+1) (index::acc) 
      else loop arr value (index+1) acc
    | _ -> acc
  in loop arr value 0 []

(** [check_dup_row] is a list of indices in [row] of [curr] with value 
    [value]. *)
let check_dup_row row value curr = 
  let indices = find_index2 curr.(row) value in
  let rec loop indices row acc= 
    match indices with
    | [] -> acc
    | hd::tl -> loop tl row ((row, hd)::acc) in
  loop indices row []

(** [check_dup_col] is a list of indices in [col] of [curr] with value 
    [value]. *)
let check_dup_col col value curr = 
  let indices = find_index2 (col_vals curr col) value in
  let rec loop indices col acc= 
    match indices with
    | [] -> acc
    | hd::tl -> loop tl col ((hd, col)::acc) in
  loop indices col []

(** [check_dup_sq] is *)
let check_dup_sq row col value curr =
  let indices = find_index2 (get_square row col curr) value in
  let rec loop indices row col acc = 
    match indices with
    | [] -> acc
    | hd::tl ->
      let row = row - row mod 3 + hd/3 in 
      let col = col - col mod 3 + hd mod 3 in 
      loop tl row col ((row, col)::acc) in
  loop indices row col []

let check_duplicates coord curr =
  let row  = fst coord in 
  let col = snd coord in
  let curr = revert_curr_board curr in
  let value = curr.(row).(col) in
  List.filter (fun elt -> elt <> coord)
    begin 
      (check_dup_row row value curr)@(check_dup_col col value curr)
      @(check_dup_sq row col value curr)
    end

let get_hint curr init= 
  let rand_coor = random_coor (revert_curr_board curr) true in
  let row = fst rand_coor in 
  let col = snd rand_coor in
  curr.(row).(col) <- (init.(row).(col), false);
  (curr, (row, col))

let unfilled_curr_board curr =
  let copy = Array.make_matrix 9 9 (0, false) in 
  for i = 0 to 8 do 
    for j = 0 to 8 do 
      if snd curr.(i).(j) == true then 
        copy.(i).(j) <- (0, true)
      else copy.(i).(j) <- (fst curr.(i).(j), false);
    done;
  done;
  copy
