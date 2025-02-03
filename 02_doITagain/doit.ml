(*
Key Concepts:
Recursive Thinking : Solving a problem by solving smaller instances of the same problem.
Base Case : The simplest case that can be solved directly (e.g., an empty list).
Recursive Case : Breaking the problem into smaller subproblems and combining the results.
Functions on Lists : Recursive functions like length, sum, and member are introduced.
We’ll implement these ideas in OCaml using pattern matching and recursion.
*)

(* The length function that calculates the elements in the list *)
let rec length_n alist =
  match alist with
  | [] -> 0
  | x :: xs -> 1 + length_n xs

(* length function with tail recursion *)
let rec l_acc n alist =
  match alist with
  | [] -> n
  | x :: xs -> l_acc (n + 1) xs

let length alist = l_acc 1 alist

(* Sum function calculates the sum of all numbers in a list *)
let rec sum_n alist =
  match alist with
  | [] -> 0
  | x :: xs -> x + sum_n xs

(* sum with tail recursion *)
let rec s_acc n alist =
  match alist with
  | [] -> n
  | x :: xs -> s_acc (n + x) xs

let sum alist = s_acc 0 alist

(* member function that checks if a given element is present in a list *)
let rec member n alist =
  match alist with
  | [] -> false
  | x :: xs ->
    if x = n then true
    else member n xs

(* Example usage *)
let found = member 3 [1; 2; 3; 4];;  (* Returns true *)
let not_found = member 5 [1; 2; 3; 4];;  (* Returns false *)

(* append function concatenates *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | x :: xs -> x :: append xs lst2

let append2 lst1 lst2 = lst1 @ lst2

(* reverse function *)
let rec reverse alist =
  match alist with
  | [] -> []
  | x :: xs -> reverse xs @ [x]

(* map function *)
let rec map func lst =
  match lst with
  | [] -> []
  | x :: xs -> func x :: map func xs

let rec filter func lst =
  match lst with
  | [] -> []
  | x :: xs -> 
    if func x then x :: filter func xs
    else filter func xs
