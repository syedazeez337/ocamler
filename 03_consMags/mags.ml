(*
Key Concepts:
Building Lists : Using :: to construct new lists.
Recursive List Manipulation : Removing elements, inserting elements, and transforming lists.
Base Case and Recursive Case : As always, recursive functions need a base case (e.g., an empty list) and a recursive case.
*)

(* rember function to remove a member from a list *)
let rec rember elem alist =
  match alist with
  | [] -> []
  | x :: xs ->
    if x = elem then rember elem xs
    else x :: rember elem xs

(* The insertR function inserts a new element to the right of the first occurrence of a given element. *)
let rec insertR r_elem l_elem alist =
  match alist with
  | [] -> []
  | x :: xs ->
    if x = l_elem then x :: r_elem :: xs
    else x :: insertR r_elem l_elem xs

(* insertL insert left of the element *)
let rec insertL r_elem l_elem alist =
  match alist with
  | [] -> []
  | x :: xs ->
    if x = l_elem then r_elem :: x :: xs
    else x :: insertL r_elem l_elem xs

(* substitute function *)
let rec subst n_elem o_elem alist =
  match alist with
  | [] -> []
  | x :: xs ->
    if x = o_elem then n_elem :: xs
    else x :: subst n_elem o_elem xs