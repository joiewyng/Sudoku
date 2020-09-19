(**
   Parsing of player commands.
*)

(** [coord_and_val] represents the coordinates of box being 
    changed as well as the value it is being changed to if the command is edit.
    The first element of the list represents the row, the second represents the 
    column and the last element represents an optional value to be assigned.  
    Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["edit (1,2,3)"], then the coord_and_val is 
      [["1"; "2"; "3"]].
    - If the player command is ["edit     (1  ,2,3)"], then the coord_and_val is
      again [["1"; "2"; "3"]].
    - If the player command is ["remove (1,2)"], then the coord_and_val is 
      [["1"; "2"]].
    - If the player command is ["remove     (1  ,2)"], then the coord_and_val is
      again [["1"; "2"]].
      The [coord_and_val] is not permitted to be the empty list. *)
type coord_and_val = int list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a coord_and_val. *)
type command = 
  | Edit of coord_and_val
  | Remove of coord_and_val
  | Hint
  | Restart
  | New
  | Time
  | Undo
  | Help
  | Quit

(** The type [level] represents a level of sudoku g. *)
type level =
  | Easy
  | Medium
  | Hard

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the coord_and_val.

    Examples: 
    - [parse "    edit   (1,2,3) "] is Edit ["1"; "2"; "3"].
    - [parse "    edit   (1 , 2 ,3) "] is Edit ["1"; "2"; "3"].
    - [parse "remove (1 , 2 )"] is Remove ["1"; "2"]. 
    - [parse "hint"] is [Hint]
    - [parse "help"] is [Help]
    - [parse "restart"] is [Restart]
    - [parse "time"] is [Time]
    - [parse "new game"] is [New]
    - [parse "undo"] is [Undo]
    - [parse "quit"] is [Quit]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is not "quit", "edit", "remove", "help", 
    "restart", "hint", or "new game"
    or if the verb is "edit" and there is an empty object phrase,
    or if the verb is "edit" and there it is not a valid coord_and_val,
    or if the verb is "remove" and there is an empty object phrase,
    or if the verb is "remove" and there it is not a valid coord_and_val
    or if the verb is "quit", "help", "restart", "hint", "undo" or "new game" 
    and there is a non-empty object phrase.
*)
val parse : string -> command

(** [parse_lvl str] parses the player's input into a level
    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. A command is malformed
    {i malformed} if the verb is not "easy", "medium", or "hard"
    or if the verb is "easy", "medium", or "hard" and there is a non-empty 
    phrase after. *)
val parse_lvl : string -> level
