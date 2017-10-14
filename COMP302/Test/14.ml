let r = ref 0;;
let s = ref 0;;
r = s;; (* True *)
r == s;; (* False *)
r := 3;;
(* Update, but it returns a unit, uninteresting
 * Always true, what it says is it succeeded
 * As an effect, it changes the value in cell x.
 * But the _value_ of updating a cell is 
 * (). unit is type, it evaluates to ()
 * Keep this in mind*)
!r;;
let x = !r + !s;; (* 3 *)

(* The following is not valid: *)
(* r := 3.4;; *)
(* This is because r is an int ref *)

r := !s;;
!r;;

r := 2+3;; (*2+3 evaluated before stored*)
!r;;

(* What's the value of r? (_IMPORTANT_)
 * The address/location in memory 
 * So t = r will set t to the same address *)
let t = r;; (* Point to same loc in mem*)
t == r;;
t := 4 * 3;;
!t;;
!r;;

(* Polymorphic functions, will see later *)
let id = ref (fun x -> x);;
id := fun x -> x + 1;;
(* Will fix x to an int *)
(* This won't work: *)
(* id := fun x -> x +. 3.2;; *)

(* Can only do something like *)
id := fun x -> x + 2;;
(* Can also overshadow references *)
let id = ref (fun x -> x + 1);;

(* Back to the area example *)
let pi = ref 3.14;;
let area r = !pi *. r *. r;;

let a2 = area (2.0);; (*12.56*)
pi := 6.0;;
let a3 = area (2.0);; (*24.0*) 
a2 = a3;; (* false *)

(* Now we can write C like
 *   programs using references *)

(* Purely functional, 
 * changes addresses in triple *)
let rot (a,b,c) = (c,b,a);;

(* Purely rotten, 
 * changes contents in triple *)
let rott (a,b,c) = let t = !a in (a := !c ; c := t ; (a,b,c)) ;;

let triple = (ref 1, ref 2, ref 3);;
rot triple;;
rott triple;;
(* They don't both do the same thing,
 * since one changes addresses vs contents *)


(*Imperative factorial
 * More complicated than purely functional ver
 * Considered bad style in functional
 * Harder to reason about its correctness 
 * Harder to understand*)
let imperative_fact n =
  begin
    let result = ref 1 in
    let i = ref 0 in
    let rec loop () =
      if !i = n then ()
      else (i := !i + 1; result := !result * !i; loop ())
    in
    (loop (); !result)
  end
    
