(* append: 'a list -> 'a list -> 'a list *)
let rec append l k = match l with
  | [] -> k
  | h::t -> h::(append t k)

(* Tail recursive *)
(* app_tl: 'a list -> 'a list -> ('a list -> 'a list) -> 'a list *)
(* First 'a list in c is "waiting for result" of rec call 
   and last one is final result *)
let rec app_tl l k c = match l with
(* | [] -> ?
 * | h::t -> app_tl t k ? *)
(* How to do this? Need something with a hole, i.e. a function *)
  (* When you do app [1;2] [3;4] ([3;4 = k]) -> 1::app [2] [3;4] 
   * -> 1::2::app [] [3;4] What to give to c here? [3;4]*)
  (* c = (fun r -> 1::2::r) *)
  (* Parameterized function, can call and start using *)
  (* c k -> 1::2::[3,4] *)
  | [] -> c k (* Calling the continuation 
               - passing to the call stack k*)
  (* What to put here? *)
  (* | h::t -> app_tl t k (fun r -> h :: c r) *)
  (* This gives reverse order *)
        
  (* Building up the call stack: *)
  (* app_tr [1;2] [3;4] (fun r -> r) (initial continuation), ident 
   -> app_tr [2] [3;4] (fun r1 -> (fun r->r) (1::r1))
   -> app_tr [] [3;4] (fun r2-> (fun r1 -> (fun r->r) (1::r1))) (2::r2)
   Collapsing the call stack
   -> (fun r2 -> (fun r1 -> (fun r -> r) (1::r1)) (2::r2)) [3,4]
   -> (fun r1 -> (fun r-> r) (1::r1)) [2;3;4] 
   -> (fun r->r) [1;2;3;4] -> [1;2;3;4] *)
  | h::t -> app_tl t k (fun r -> c (h::r))
          
let rec genList n acc =
  if n > 0 then genList (n-1) (n::acc) else acc;;

let l1 = genList 8000000 [];;
let l2 = genList 4000000 [];;

(* append l1 l2 gives stack overflow*)
(* So does l1 @ l2 
 * Ocaml didn't implement it through tail-recursion
 * For short lists, it works faster
 * But it cannot append huge lists like this one
 * Program can crash vs program being a bit slower*)
app_tl l1 l2 (fun r -> r);;

let rec map l f = match l with
  | [] -> []
  | h::t -> (f h)::map t f

(* Past interview question, wanted to reimplement map tail recursively *)

let map' l f =
  let rec map_tl l f c = match l with
    | [] -> c []
    (* Build up calling stack *)
    | h::t -> map_tl t f (fun r -> c ((f h):: r))
  in
  map_tl l f (fun r -> r)
