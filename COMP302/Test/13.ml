(* Question 1 *)
(* i. *)
fun y -> 3 * y;;
(* Type? int -> int *)
(* Evaluates to? <fun> *)

(* ii. *)
fun (x,y) -> if x then y else 2.0;;
(* bool * float -> float *)
(* <fun> *)
(* iii. *)
let x = 4 in let y = x * 2 in let x = 3 in x + y;;
(* int *)
(* 11 *)

(* iv. *)
fun x y -> [x ; y];;
(* 'a -> 'a -> 'a list *)
(* <fun> *)

(* v. *)
fun l -> match l with [] -> true | _ -> false;;
(* 'a list -> bool *)
(* <fun> *)

(* Question 2 *)
type suit = Clubs | Spades | Hearts | Diamonds
type rank = Two | Three | Four | Five | Six | Seven | Either | Nine | Ten | Jack | Queen | Kind | Ace

let rec sort (p:card -> card -> bool) (h:hand) = match h with
  | Empty -> Empty
  | Hand (c, Empty) -> h
  | _ -> let (h1,h2) = split h in merge p (sort p h1) (sort p h2)

(* Assume sort is given *)

(* Write split *)
let rec split = function
  | Empty -> Empty, Empty
  | Hand(c,Hand(d,h')) ->
     let h1,h2 = split h' in
     Hand(c, h1),Hand(d,h2)
  | Hand(c, Empty) -> Hand(c, Empty), Empty

(* Question 3 *)
type price = float
type weight = float
type calories = int
type ingredient = Nuts | Gluten | Soy | Dairy
type cup_cake = Cup_cake of price * weight * calories * ingredient list

                                                                   (* Implement allergy_free: ingredient list -> cup_cake list -> cup_cake list          
Returns the list of cupcakes that don't have these ingredients*)
(* Use: List.filter, List.exists, List.for_all (types are given) *)
let allergy_free allergens l = List.filter(fun (Cup_cake(_,_,_,is)) -> not(List.exists(fun i -> List.mem i is) is )) l

(* Alternate solution  *)
let allergy_free' allergens l = List.filter(fun (Cup_cake(_,_,_,il)) -> (List.for_all (fun ing -> not(List.exists (fun allerg -> ing = allerg) allergens)) il)) l

                                  
(* Question 4 *)
(* count h = count_tr h 0 *)
(* For counting amount of cards in a hand*)
(* Show other theorem first, count h+acc = count_tr h acc *)
let rec count h = match h with
  |Empty -> 0
  |Hand (c,h) -> count h + 1

let rec count_tr h acc = match h with
  |Empty -> acc
  |Hand(c,h) -> count_tr h (1+acc)
