(* BNF notation *)
type regexp =
  Char of char | Times of regexp * regexp | One | Zero |
  Plus of regexp * regexp | Star of regexp

(* How to write? *)
(* String s: 
 * Never matches 0
 * Matches 1 if s = empty 
 * Matches a iff s = a
 * Matches r1+r2 iff s matches r1 or r2
 * Matches r1 r2 iff s1 = s1 s2 
   with s1 matching r1 and s2 matching r2
 * Matches r* iff s= empty or s = s1 s2 
   with s1 matching r1 and s2 matches r* *)

(* acc: regexp -> char list (each char that makes up string) 
   -> (char list -> bool) (success continuation) -> bool *)
(* Accumulating things on call stack, 
   return type of success continuation must be 
   the same as return type of acc, i.e. bool *)
(* acc (Times(r1,r2)) s *)
(* Idea: check if a prefix of s matches r1 *)
(* But we still need to check the remaining part of s with r2 
 * So we pass the whole char list to k *)
(* We use continuations as backtracking, so we don't have to 
   worry about where we split the string *)
let rec acc r clist k = match r, clist with
  | Char c, [] -> false (* Expected some characters, got nothing *)
  | Char c, c1::s -> (c = c1) && k s (* At the bottom of the stack
                                        If we succeed, pass rest of string to                                      call stack*)
  | Times (r1, r2), s -> acc r1 s (fun s2 -> acc r2 s2 k)
  (* Can we match remaining string as well? 
Pass to continuation and also call k, whatever it still has to do*)
  | One, s -> k s (* Nothing to do, call continuation with whole string *)
  | Plus (r1, r2), s -> acc r1 s k || acc r2 s k (*Only has to be 1*)
  | Zero, s -> false
  | Star r, s ->
     (k s) || acc r s (fun s2 -> not (s = s2) && acc (Star r) s2 k)
(* Make sure we're consuming something, s2 is smaller *)
(* Note that (ap* )(l (e+g)) is represented as: 
 * Times(Times(Char "a", Star(Char "p")),Times(Char "l",Plus(Char "e", Char "y")))*)

let string_explode s =
  tabulate (fun n -> String.get s n) ((String.length s) - 1)

let string_implode l =
  List.fold_right (fun c s -> Char.escape c ^ s) l ""

                  (* let accept r s =
                   *   acc r string_explode s  *)
