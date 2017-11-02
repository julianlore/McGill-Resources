type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf n = Node (Empty, n, Empty)

let r = Node (leaf 22, 35, leaf 70)
let ll = Node (leaf 3, 5, leaf 7)
let l = Node(ll, 9, leaf 15)
let t = Node (l, 17, r)

(* Contrasting three different versions *)
(* Using options *)
(* find: ('a -> bool) -> 'a tree -> 'a option *)
(* Good exercise to understand a function, write types *)
let rec find p t = match t with
  | Empty -> None
  | Node (l, d, r) ->
     if (p d) then Some d
     else (match find p l with
           | None -> find p r
           | Some d' -> Some d')
(* Kind of messy and overcomplicated 
 * Building up a call stack that's useless
 * Have to go down all of l and then only
 * r when we're done with l*)

    
(* Using exceptions *)
exception Fail

let rec find_ex p t = match t with
  | Empty -> raise Fail
  | Node(l,d,r) -> if (p d) then Some d
                   else (try find_ex p l with Fail -> find_ex p r)
(* If you fail, backtrack on right tree *)
let find' p t =
  (try find_ex p t with Fail -> None)


(* Using failure continuation *)
(* Continuation: Base case, want to call call stack *)
(* In this case it's c *)
(* find_cont ('a -> bool) -> 'a tree -> (unit -> 'a option) -> 'a option *)
let rec find_cont p t c = match t with
  | Empty -> c ()
  (* Instead of raising and causing an effect here, we call continuation stack *)
  | Node(l,d,r) -> if (p d) then Some d
                   else find_cont p l (fun () -> find_cont p r)
(* If left fails, then we go on left side 
 * Building up call stack*)
                 
let rec find'' p t = find_cont p t (fun () -> None)
                               (* If we're at the very last leaf, 
   will pass unit to call stack and will get None 
 * Could have also made a fail exception*)


find'' (fun x -> x = 22) t;;
(* find_cont p t (fun (0 -> None))
 * -> find_cont p t (fun () -> None) 
 * If this fails
 * -> find_cont p l (fun () -> find_cont p r (fun () -> None))
 * Building up stack in case going down left fails
 * Passing it up and getting ready for next time like with exceptions
 * -> find_cont p ll (fun () -> find) cont p lr ....
 * This consists of building up the call stack, remember what to do upon fail*)

(* Good reason to use this is to make it tail-recursive and not get out of memory problems *)

(* Finding all elements satisfying a given property *)
(* Recursive *)
(* findAll: ('a -> bool) -> 'a tree -> 'a list *)
let rec findAll p t = match t with
  | Empty -> []
  | Node (l, d, r) ->
     let el = findAll p l in
     let er = findAll p r in
     if (p d) then el @ (d :: er)
     else el @ er;;

findAll (fun x -> x mod 3 = 0) t;;

(* Continuations, but this time on success to build up result *)
let rec findAll' p t sc = match t with
  | Empty -> sc []
  | Node(l, d, r) ->
     if (p d) then
       findAll' p l (fun el -> findAll' p r (fun er -> sc (el @ (d::er))))
     else
       assert false
              (* Figure this out yourself *)
