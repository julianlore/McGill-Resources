(* type mylist = Nil | Cons of ? * list;; *)
(* Polymorphic lists:  *)
(* type 'a mylist = Nil | Cons of 'a * 'a my list *)
[] ;;
1 :: [] ;;
1 :: 2 :: 3 :: [] ;;
[1;2;3;4];;

(* These are only homogenous lists though, what if we want floats and ints? *)
(* type if_list = Nil | ICons of int * if_list | FCons of float * if_list *)
(* But here we can't use List libraries *)

(* So make an element that can be either *)
type elem = I of int | F of float;;

let rec append l1 l2 = match l1 with
  | [] -> l2
  | x::xs -> x :: append xs l2;;
(* Program execution *)
(* append 1::(2::[]) -> 1 :: append (2::[])             *)
let head l = match l with
  | [] -> None
  | x :: xs -> Some x;;

(* Write a function rev given a list l of type 'a list returns it's reverse  *)
                 
(* Silly way of doing this             *)
let rec rev (l : 'a list) = match l with
  | [] -> []
  | hd :: tail -> rev (tail) @ [hd];;
(* Could we have written rev(tail) :: hd? No. Why? *)
(* a' : 'a list, left side has to be one element, right side  to be a list *)
(* 'a list @ 'alist *)

(* What is the type of rev? Is it 'a list? -No *)
(* It is 'a list -> 'a list *)

(* Is this a good program? Long running time, use tail recursion  *)


let rev_2 (l : 'a list) =  
  let rec rev_tr l acc = match l with
    | [] -> acc
    | h::t -> rev_tr t (h::acc)
  in
  rev_tr l [];;

(* Exercises:
 * Write a function merge: 'a list -> 'a list -> 'a list
which given ordered lists l1 and l2, both of type 'a list,
it returns the sorted combination of both lists 
 * Write a function split: 'a list -> 'a list * 'a list 
which given a list l it splits into two sublists, 
(every odd element, every even element)*)
