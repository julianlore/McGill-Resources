exception Domain

let fact n =
  let rec f n =
    if n = 0 then 1
    else n * f (n-1)
  in
  if n < 0 then raise Domain
  else f(n)

let runFact n =
  try
    let r = fact n in
    print_string ("Factorial of "^ string_of_int n ^
                    " is " ^ string_of_int r ^ "\n")
  with Domain ->
    print_string("Error: Trying to call factorial on a negative input \n");;

fact 0;;
fact (-1);;

(* let fact' n =
 *   let rec f n =
 *     if n = 0 then 1
 *     else n * f (n-1)
 *   in
 *   if n < 0 then raise (Error "Invalid Input")
 *   else f(n)
 * 
 * let runFact' n =
 *   try
 *     let r = fact n in
 *     print_string ("Factorial of "^ string_of_int n ^
 *                     " is " ^ string_of_int r ^ "\n")
 *                  (\* with Error msg ->
 *                   *   print_string(msg ^ "\n");; *\)
 *                  (\* Can pattern match here too *\)
 *   with Error "Invalid Input" -> print_string ("Programmer says you passed an invalid input \n")
 *      | Error msg -> print_string(msg ^ "\n") *)

type key = int

type 'a btree =
  | Empty
  | Node of 'a btree * (key * 'a) * 'a btree



(* let l = Node (Node (Empty, (3, "3"), Empty), (7,"7"),
 *               Node (Empty, (4, "4")  *)
          
(* Binary search tree searching *)
exception NotFound
(* Can use exceptions for positive things as well *)
exception Found of int
                 

(* let rec findOpt1 t k = match with
 *   | Empty -> raise NotFound
 *   | Node(l, (k',d),r) ->
 *      if k = k' then raise (Found d)
 *      else
 *        (if k < k' then findOpt1 l k else findOpt1 r k) *)

(* Now we don't assume that the tree is a binary search tree *)
let rec findOpt t k = match t with
  | Empty -> None
  | Node(l, (k',d), r) ->
     if k = k' then Some d
     else
       (match findOpt l k with
        | None-> findOpt r k
        | Some d -> Some d)

(* Doing it with exceptions *)
    
let rec find t k = match t with
  | Empty -> raise Not_Found
  | Node (l, (k', d), r) ->
     if k = k' then d
     else try (find l k with NotFound -> find r k) 
