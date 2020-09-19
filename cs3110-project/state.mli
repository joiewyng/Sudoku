(** 
   Representation of dynamic sudoku state.

   This module represents the state of the sudoku board as it is being played,
   including the managing the hints, mistakes, and undo, updating and removing 
   the values in the sudoku cells, restaring or creating a new game, 
   and functions that cause the state to change.
*)

(** The abstract type of values representing the sudoku state. *)
type t

(** The type representing the result of a move. *)
type result = Valid of t | Invalid of string

(** [init_state] is the initial state of the sudoku game. *)
val init_state : Board.level -> t

(** [full_board st] is the full sudoku board in state [st]. *)
val full_board : t -> Board.init_board

(** [current_board st] is the current sudoku board in state [st]. *)
val current_board : t -> Board.current_board

(** [hint st] is the number of hint left and coordinate of hint given board 
    in state [st]. *)
val hint : t -> int * Board.coordinate option

(** [mistakes st] is the number of mistakes left in state [st]. *)
val mistakes : t -> int 

(** [update_cell coord value st] is result of attempting to place [value] in 
    coordinate [coord] of the sodoku board. *)
val update_cell : Board.coordinate -> int -> t -> result

(** [update_cell coord value st] is result of attempting to remove value in 
    coordinate [coord] of the sodoku board. *)
val remove_cell : Board.coordinate -> int -> t -> result

(** [give_hint st] is result of attempting to give player a hint. *)
val give_hint : t -> result

(** [deduct_mistake st] is result of attempting to deduct the number of 
    mistakes the player can still make. *)
val deduct_mistake : t -> result

(** [undo_moves st] is result of attempting to undo the previous move. *)
val undo_moves : t -> result 

(** [win st] is whether the player completed the sudoku correctly. *)
val win : t -> bool

(** [restat_game st] is state with the same sudoku game restarted . *)
val restart_game : t -> t
