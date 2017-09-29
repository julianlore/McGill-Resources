type 'a tree = Empty | Node of 'a = 'a tree * 'a tree;;
let t0 = Node (5, Node (3, Empty, Empty), Empty);;
(* 5 is at the head, has 3 as left child, all other children are empty *)

let rec size t = match t with
  | Empty -> 0
  | Node (v, l, r) -> 1 + size l + size r

                                     size t0;;
(* Since Ocaml is a stack, 
  if you modify the tree type after declaring t0,
  then t0 will be the old type, 
  so when you try to use size you'll get an error 
  as it's not the same type *)

type 'a forest = Forest of ('a many_trees) list
and 'a many_trees = Empty | MoreTrees of 'a many_trees

(* Mutually recursive *)
                                       
let rec size_forest f = match f with
  | Forest trees -> match trees with
                    | [] -> 0
                    | h::t -> size_many_trees h + size_forest (Forest t) 

and size_many_trees t = match t with
  | NoTree -> 0
  | MoreTrees f -> 1 + size_forest f
