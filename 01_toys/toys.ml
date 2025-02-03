(* Atom -> integers, string, or oth primitive types *)
let atom = 42
let string_atom = "Hello"

let () = Printf.printf "%d, %s" atom string_atom

(* Square brackets to represent lists *)
let my_list = [1;2;3;4;5]
let empty_list = []

(* `car` -> Head of the list *)
let hd = List.hd my_list

let head lst =
    match lst with
    | [] -> failwith "Empty list has no head"
    | x :: xs -> x


(* `cdr` -> tail of the list *)
let tl = List.tl my_list

let tail lst =
    match lst with
    | [] -> failwith "Empty list has no tail"
    | x :: xs -> xs

(* Constructing a list `cons` operator :: *)
let a_list = 1 :: 2 :: 3 :: 4 :: 5 :: []

(* Checking if something is an Atom *)
type value =
    | Atom of int
    | List of value list

let is_atom x =
    match x with
    | Atom _ -> true
    | List _ -> false

(* checking if something is a list *)
let is_list (x : 'a list option) =
    match x with
    | Some [] -> true  (* Empty list *)
    | Some (_ :: _) -> true  (* Non-empty list *)
    | None -> false;;  (* Not a list *)