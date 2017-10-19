type 'a rlist = Empty | RCons of 'a * ('a rlist) ref

let l1 = ref (RCons (4, ref Empty))
let l2 = ref (RCons (5, l1));;

l1 := !l2;;
(* Value is (), effect is changing link to itself *)

(* Append for regular lists *)
let rec append l1 l2 = match l1 with
  | [] -> l2
  | x::xs -> x::(append xs l2)

(* Append for rlist *)
type 'a refList = ('a rlist) ref
(* Return unit, as the "result" is the effect *)
(* 'a refList->'a refList->unit *)
let rec rapp (r1 : 'a refList) (r2 : 'a refList) = match r1 with
  | {contents = Empty} -> r1 := !r2
  | {contents = RCons (x, xs)} -> rapp xs r2

(* 'a refList -> 'a refList -> 'a rlist *)
let rec rapp' (r1 : 'a refList) (r2 : 'a refList) = match r1 with
  | {contents = Empty} -> {contents = r2}
  | {contents = RCons (x, xs)} -> rapp' xs r2
                                
let r = ref (RCons (2, ref Empty))
let r2 = ref (RCons(5, ref Empty));;


let r3 = rapp' r r2;;
r3;;
rapp r r2;;
r;;

let (tick, reset) =
  let counter = ref 0 in
  (* Input is unit, always true. Not the same as void *)
  let tick () = (counter := !counter + 1 ; !counter) in
  let reset () = counter := 0 in
  (tick, reset);;


(* Now we have 2 functions, tick and reset *)
tick ();;
tick ();;

type counter_obj = {tick : unit -> int ; reset : unit -> unit}

let makeCounter () =
  let counter = ref 0 in
  {tick = (fun () -> counter := !counter + 1 ; !counter);
   reset = (fun () -> counter := 0)};;

(* global variable *)
let global_counter = ref 0
let makeCounter' () =
  let counter = ref 0 in
  {tick = (fun () -> counter := !counter + 1 ; global_counter := !counter ; !counter);
   reset = (fun () -> counter := 0)};;

let c = makeCounter ();;
c.tick ();;
c.tick ();;
let d = makeCounter ();;
d.tick ();;
c.tick ();;
d.reset ();;
