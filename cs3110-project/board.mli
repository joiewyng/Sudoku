(**
   Board of Sudoku
*)

(** The type representing a full sudoku board *)
type init_board = int array array

(** The type representing a sudoku board with hidden values *)
type current_board = (int*bool) array array

(** The type representing a coordinate on the board *)
type coordinate = int*int

(** The type representing levels of game *)
type level = 
  | BEasy
  | BMedium
  | BHard

(** Raised when coordinate is invalid. *)
exception InvalidCoordinate of coordinate

(** Raised when value is invalid. *)
exception InvalidValue of int

(** [create_init_board] initalizes a 9x9 board *)
val create_init_board : unit -> init_board

(** [create_curr_board lvl] is a board for player *)
val create_curr_board : init_board -> level -> current_board

(** [update coord value] is the board with [value] at coordinates [row],[col] 
    Raises: 
    - InvalidCoordinate [coord] if (row, col) in board [curr] is not 
      editable. 
    - Invalid Value [value] if [value] is not in range 1-9 *)
val update : coordinate -> int -> current_board -> current_board

(** [remove coord] is the board with [value] at coordinates [row],[col] 
    Raises: InvalidCoordinate [coord] if (row, col) in board [curr] is not 
    editable *)
val remove : coordinate -> current_board -> current_board

(** [check_duplicates coord curr] is a list of indices in the same row, col, or 
    square as [coord] in [curr] that have the same value as [coord] *)
val check_duplicates : coordinate -> current_board -> coordinate list

(** [get_hint curr init] is a board with a random zero coordinate value 
    filled in with the correct value *)
val get_hint : current_board -> init_board -> current_board * coordinate

(** [unfilled_curr_board curr] is an array with original entries of [curr] *)
val unfilled_curr_board : current_board -> current_board
