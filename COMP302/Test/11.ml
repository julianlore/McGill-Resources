(* Write a function curry that takes as input 
 * a function f:('a * 'b)->c' 
 * and returns as a result a function
 * 'a->'b->'c*)

(* curry : (('a * 'b)->'c)-> 'a -> 'b -> 'c 
 * Note: Arrows are right-associative. *)
let curry f = (fun x y -> f (x,y))

let curry_version2 f x y = f (x,y)

let curry_version3 = fun f -> fun x -> fun y -> f (x,y)

                                                  (* Uncurry *)
                                                  (* uncurry ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)
let uncurry f = (fun (x,y) -> f x y)

(* swap : ('a * 'b -> 'c) -> 'b * 'a -> 'c *)
let swap f = fun (b, a) -> f(a , b)                  

let plus' (x,y) = x + y

(* swap plus' ===> fun (b,a) -> plus' (a,b) *)
