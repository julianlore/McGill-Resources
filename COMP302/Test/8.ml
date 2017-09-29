(* Data Types: Trees *)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let rec size t = match t with
  | Empty -> 0
  | Node (v, l, r) -> 1 + size l + size r 
let rec insert ((x,dx) as e) t = match t with
  (* Tree is empty, root is now e *)
  | Empty -> Node (e, Empty, Empty)
  | Node ( (y,dy), l, r) ->
     (* Replace val that has same key *)
     (* No destructive updates, need to keep elements and remake tree *)
     if x = y then Node (e, l, r)
                        (* Go down left tree *)
     else (if x < y then Node ( (y,dy), insert e l, r)
                              (* Go down right tree *)
           else Node ((y,dy), l, insert e r)
          )
            
            (* Can we still use these less than signs for any type? 
             The node constructor uses any type
             Since we used comparison, we can*)

;;
3 < 4 ;;
Empty < Node (3, Empty, Empty);;
Node (3, Empty, Empty) < Node (4, Empty, Empty) ;;

[3 ; 4] < [2 ; 5];;
(* Why is this false? *)

[3 ; 5] < [4 ; 7];;
[3 ; 5] < [7];;
(* Doesn't look at length of list, looks at first number of list *)
(* Dangerous to have comparison on all these types. Can only compare built in data types (for lists here, it's comparing ints) 
 OCaml will come up with something, but it might not be the correct thing
 For example, with the suits example we made a function to quantify
 what's bigger*)

(* lookup: 'a -> ('a x 'b)tree -> b' option *)
(* Option in case key isn't there *)
let rec lookup x t = match t with
  | Empty -> None
  | Node ( (y, dy), l, r) ->
     if x = y then Some dy
     else ( if x < y then
              lookup x l
            else lookup x r
          )
;;


(* collect: 'a tree -> 'a list 
 What order do we want to return it in? In order traversal*)
let rec collect t = match t with
  | Empty -> []
  | Node (x, l, r) ->
     let l1 = collect l in
     let l2 = collect r in
     l1 @ l2
(* Incomplete, where to put x? *)
;;

collect (Node (5, Node (3, Empty, Empty), Empty));;
