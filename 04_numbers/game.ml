(*
Key Concepts:
Recursive Arithmetic : Solving arithmetic problems by breaking them into smaller subproblems.
Base Case : The simplest case that can be solved directly (e.g., 0).
Recursive Case : Breaking the problem into smaller subproblems and combining the results.
*)

(* add function using recursion *)
let rec add m n =
  match n with
  | 0 -> m
  | _ -> add (m + 1) (n - 1)

(* multiply function using recursion *)
let rec multiply m n =
  match n with
  | 0 -> m
  | _ -> add m (multiply m (n - 1))

(* exponentiation function raises a number to a power recursively *)
let rec expo m n =
  match n with
  | 0 -> 1
  | _ -> multiply m (expo m (n - 1))

(* Subtraction function *)
let rec subtract m n =
  match n with
  | 0 -> m
  | _ -> subtract (m - 1) (n - 1)

(* count function *)

let rec acc_c n alist =
  match alist with
  | [] -> n
  | x :: xs -> acc_c (n + 1) xs

let count alist =
  acc_c 0 alist