();;
(* unit *)

fun x -> x := 3;;
let y = ref 1;;
(fun x -> x := 3) y;;
y;;
let x = ref 1 in
    fun x -> (x := 3; x);;

(* Linked list *)
type 'a rlist = Empty | RCons of 'a * ('a rlist) ref;;
let l1 = ref (RCons (4, ref Empty));;
let l2 = ref (RCons (5, l1));;
(* The 'a rlist ref of l2 is l1, same address *)

(* What happens here? *)
l1 := !l2;;
(* We have created a circular list *)
!l1;;
