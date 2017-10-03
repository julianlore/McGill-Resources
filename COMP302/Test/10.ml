(* Common built in higher-order functions we'll be writing *)

(* map: ('a -> 'b) -> 'a list -> list, bracket does whatever function f does *)
(* map is the most important higher order function *)
let rec map f l = match l with
  | [] -> []
  (* Apply function to head and then prepend to what you get from recursive call *)
  | h :: t -> (f h) :: map f t;;

(* Increment all by one  *)
map (fun x -> x + 1) [1 ; 2 ; 3 ; 4];;

(* Convert to strings *)
map (fun x -> string_of_int x) [1 ; 2 ; 3 ; 4];;

(* filter: ('a -> bool) -> 'a list -> 'a list  
 Want to filter out elements of a list  
 function in bracket takes an argument and returns boolean whether it's good or not*)
(* Ex. filter (fun x-> x mod 2 = 0) [1 ; 2 ; 3 ; 4] should give [2 ; 4] *)
let rec filter p l = match l with
  | [] -> []
  | h :: t ->
     (* If it satisfies p, prepend to recursive call *)
     if p h then h :: filter p t 
     else filter p t;;

(* Being on the safe side, we can write: 
 * let pos l = filter (fun x -> x > 0) l *)

(* But we can also write , because it partially evaluates function
 * What we get back is a function from 'a list -> 'a list
 * and we can return a function *)
let pos = filter (fun x -> x > 0);;

pos [1 ; -1 ; 2 ; -3 ; -4 ; 7];;

(* fold_right: _f_ -> _base/init_ -> 'a list -> _result_
`* fold_right: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b 
 * Also known as reduce in some other languages
 * For example, if we want to sum over a list we'd write: *)

(* let rec sum l =
 *   let rec suma l acc = match l with
 *     | [] -> acc
 *     | h::t -> suma t (h+acc) in
 *   suma l 0;;
 * 
 * (\* sum [1 ; 2 ; 3 ; 4];; *\)
 * (\* 1+(2+(3+(4+0))) *\)
 * 
 * let rec prod l =
 *   let rec proda l acc = match l with
 *     | [] -> acc
 *     | h::t -> proda t (h*acc) in
 *   suma l 1;; *)
(* For a string, we'd concat instead of add or multiply 
 * So we want to abstract this common functionality *)

(* 1, f(2, f(3,f(4))) *)

(* fold_right f init [x1 ; .... ; xn] 
 * ==> f(x1, f(x2,... (f(xn, init))))
 * fold_right: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)

let rec fold_right f init l = match l with
  | [] -> init
  | h :: t -> f(h, fold_right f init t);;
(* Not really tail recursive, can make it tail recursive though *)


fold_right (fun (x,acc)->x+acc) 0 [1 ; 2 ; 3 ; 4 ; 5];; (* sum *)
fold_right (fun (x,acc)->x*acc) 1 [1 ; 2 ; 3 ; 4 ; 5];; (* prod *)

(* Concatenate as strings in list 
 * Convert each int to a string and use ^ operator to concatenate 2 strings
 * init is empty string*)
fold_right (fun (x,acc)->(string_of_int x) ^ acc) "" [1 ; 2 ; 3 ; 4 ; 5];;


(* Function that adds two numbers, but doesn't work with fold_right 
 * as it needs a function that takes a tuple, not 2 ints  *)
(+) 3 4;;

(* Folds the other way, will see difference with String function, 
 * but not with commutative things like addition
 * fold_left f init [x1 ; .... ; xn] ==> f(xn, (f (xn-1, ... (f (x1, init)))))
 * fold_left: ('a * b' -> 'b) -> 'b -> 'a list -> 'b*)

let rec fold_left f init l = match l with
  | [] -> init
  | h::t -> fold_left f (f (h, init)) t;;

fold_left (fun (x,acc)->(string_of_int x) ^ acc) "" [1 ; 2 ; 3 ; 4 ; 5];;

(* for_all p l returns true if all elements in l satisfy p *)
(* let rec for_all p l  *)

(* exists p l returns true if there exists an elements in l satisfy p *)

(* Things in basic library *)
List.map;;
List.fold_right;;
List.fold_left;;
List.filter;;
List.for_all;;
List.exists;;
(* etc *)
(* Writing these functions is good practice *)
