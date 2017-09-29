(* Arbitrary functions *)
(* cube, rcube, square, exp, sumInts, sumSquare, sumCubes, sumExp *)
let square x = x * x;;
let cube x = x * x * x;;
let rec exp (a, b) = match a with
  | 

  (* Non-generalized sums  *)
  let rec sumInts (a,b) = if (a > b) then 0 else a + sumInts(a+1,b);;
let rec sumSquare(a,b) = if (a > b) then 0 else square(a) + sumSquare(a+1,b);;
let rec sumCubes(a,b) = if (a > b) then 0 else cube(a) + sumCubes(a+1, b);;

                          
(* We will abstract over the function f (i.e. cube, square, exp etc) 
 to get a general sum function*)

(* sum: (int -> int) -> int * int -> int  *)
let rec sum f(a,b) =
  if a > b then 0
  else f(a) + sum f(a+1, b);;

(* Call function on a *)

(* Identity function, returns Argo *)
let id x = x;;
let exp2 x = exp (2, x);;

(* let sumInts' (a,b) = sum id (a,b);; *)
(* anonymous functions *)
let sumInts' (a,b) = sum (fun x -> x) (a,b);;

(* let sumSquare' (a,b) = sum square(a,b);; *)
let sumSquare' (a,b) = sum (fun x -> x * x) (a,b)

let sumCubes' (a,b) = sum cube(a,b);;

(* let sumExp' (a,b) = sum exp2(a,b);; *)
let sumExp' (a,b) = sum (fun x-> exp (2,x)) (a,b);;

(* Inconvenient, we have to define a function beforehand *)
(* How can we define a function on the fly without naming it?
 -> Use anonymous functions*)

(* Different ways to make anonymous functions *)
fun x y -> x + y;;
function x -> x;;
fun x -> x;;
(* Can use function for pattern matching 
 Don't need to write match
 Function can only take in one argument and implies pattern matching
 fun can take many *)
(function 0 -> 0 | n -> n+1);;
(* Equivalent to fun and match *)
(fun x -> match x with 0 -> 0 | n -> n+1);;


(* comb: is how we combine - either * or + 
 f : is what we do to the a
 inc : is how we increment a to get to b
 base : is what we return when a > b *)
(* Make this tail recursive this time  *)
let rec series comb f (a,b) inc base =
  if a > b then base
  else series comb f (inc(a),b) inc (comb base (f a));;
(* Base acts as an accumulator *)
