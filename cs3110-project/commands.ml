type coord_and_val = int list

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

type level =
  | Easy
  | Medium
  | Hard

exception Empty
exception Malformed

(**[filter_coor str] is the coordinate of the box being editted and/or the value
   the box is being changed to.

   Examples: 
   - ["    edit   (1;2;3) "] is  ["1"; "2"; "3"]
   - ["    edit   (1 ; 2 ;3) "] is  ["1"; "2"; "3"]
   - ["remove (1 ; 2 ; 3)"] is ["1"; "2"; "3"]. 

   Raises: [Malformed] if [str] does not contain parentheses or semicolon.
*)
let filter_coor str = 
  try
    let trimmed = String.trim(str) in
    let first = String.index trimmed '(' in 
    let last = String.index trimmed ')' in 
    String.sub trimmed (first+1) (last - first-1) 
    |> String.split_on_char ','
    |> List.map (fun x -> (String.trim(x))) 
    |> List.filter (fun x -> x <> "" )
  with e -> raise Malformed

(** [parse_helper_one_word hd] is the command when the player has a one word 
    input *)
let parse_helper_one_word hd = 
  match hd with 
  | "quit" -> Quit 
  | "hint" -> Hint
  | "help" -> Help
  | "time" -> Time
  | "undo" -> Undo
  | "restart" -> Restart
  | _ -> raise Malformed

(** [parse_helper_one_word hd] is the command edit or remove *)
let parse_edit_remove command = 
  match command with 
  | [] -> raise Empty
  | hd::tl ->
    try
      if hd = "edit" && (List.length tl = 3) 
      then Edit (List.map (fun x -> int_of_string x) tl)
      else if hd = "remove" && (List.length tl = 2) 
      then Remove (List.map (fun x -> int_of_string x) tl)
      else raise Malformed
    with e -> raise Malformed

let parse str = 
  let words = String.split_on_char ' ' (String.trim(str)) in
  let filter = List.filter (fun x -> x <> "" ) words in
  match filter with 
  | [] -> raise Empty
  | hd::[] -> parse_helper_one_word hd
  | hd::md::tl -> 
    match hd with 
    | "new"-> if md = "game" && tl=[] then New else raise Malformed
    | _ -> let command = [hd]@(filter_coor str) in
      parse_edit_remove command

let parse_lvl str = 
  let words = String.split_on_char ' ' (String.trim(str)) in
  let filter = List.filter (fun x -> x <> "" ) words in
  match filter with 
  | [] -> raise Empty
  | hd::tl -> 
    if tl != [] then raise Malformed else 
      match hd with 
      | "easy" -> Easy
      | "medium" -> Medium
      | "hard" -> Hard
      | _ -> raise Malformed