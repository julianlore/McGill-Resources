(* Warm up *)
(* Write a function lookup: 'a -> ('a * 'b) list -> 'b option. 
Given a key k of type 'a and a list l of key-value pairs, 
return the corresponding value v in l (if it exists). *)
(* lookup : 'a -> ('a * 'b) list -> 'b option  *)
let rec lookup k l = match l with
  | [] -> None
  | (k',v')::t -> if k=k' then Some v' (* If it is the right key, return val*)
                  else lookup k t;;

(* Write a function insert which
given a key k and a value v and an ordered list l of type ('a * 'b) list
it inserts the key-value pair (k,v) into the list l 
preserving the order (ascending keys). *)
(* insert : ('a * 'b) -> ('a * 'b) list -> ('a * 'b) list 
 insert (k,v) l = l'

 Precondition: l is ordered.

Postcondition: l' is also ordered and we inserted (k,v) at the right position in l*)

(* let rec insert (k,v) l = match l with
 *   | [] -> [(k,v)]
 *         (\* k = k' or k < k' or k' < k *\)
 *   | ((k',v') as h) :: t ->
 *      if k = k' then (k,v) :: l
 *      else
 *        if k' < k then (k,v) :: l
 *        else h :: insert (k,v) t;;
 * 
 * let l = [(1,"anne") ; (7,"di")];;
 * l;;
 * let l0 = insert (3,"bob") l;;
 * insert (3,"tom") l0 ;;
 * (\* But now we'll have 2 entries with the same key *\) *)
(* Undesirable, better to replace the value if its a dictionary*)

let rec insert (k,v) l = match l with
  | [] -> [(k,v)]
        (* k = k' or k < k' or k' < k *)
  | ((k',v') as h) :: t ->
     if k = k' then (k,v) :: t (* Replace *)
     else
       if k' < k then (k,v) :: l
       else h :: insert (k,v) t;;

(* Personal tail recursive attempt *)
let insert_t (k,v) l =
  let rec insert_acc (k,v) l acc = match l with
    | [] -> acc @ [(k,v)]
    | ((k',v') as h) :: t ->
       if k = k' then (k,v) :: t
       else
         if k' < k then (k,v) :: l
         else insert_acc (k,v) t (acc @ [h])
