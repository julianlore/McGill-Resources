(* We define a type of simple binary trees with data in their nodes *)
type 'a tree
  = Leaf
  | Node of 'a tree * 'a * 'a tree

(* We can write a function that counts how many nodes satisfy the predicat p *)
let rec count p = function
  | Leaf -> 0
  | Node (l, x, r) ->
     let cnt = count p l + count p r in
     if p x then cnt + 1 else cnt

(* Note that the direct version of the function is not tail recursive.
   What would it take to make it recursive? Well, a simple accumulator
   cannot be enough because we need to get rid of two recursive calls.
   What we need is a success continuation that counts how many types p
   is satisfied *)

(* Let's implement count_k that is tail recursive. Pay special
   attention to the type annotation, if we respect this type
   the resulting function will be tail recursive! *)

let rec count_k (p : 'a -> bool) (t : 'a tree) (k : int -> 'b) : 'b =  
  match t with 
  | Leaf -> k 0 (* Empty, base case gives you 0 satisfying *)
  | Node (l,x,r) -> 
     if p x  then
       (* fun m waits for result from left 
          fun n waits for result from right (collapse) 
          final result is both together 
          add 1 because it succeeds *)
       count_k p l (fun m -> count_k p r (fun n -> k (m + n + 1 )))
     else 
       count_k p l (fun m -> count_k p r (fun n -> k (m + n)))

(* Now you can write count' that behaves like count but it is tail
   recursive (of course, we will use count_k to do it) *)
let count' p t = count_k p t (fun x -> x)
(* x -> x because we just want to return the int we've been building *)

let five = Node(Leaf, 5, Leaf);;
let seven = Node(Leaf, 7, Leaf);;
let fsf = Node(five, 7, five);;
let sfs = Node(seven, 5, seven);;
let ex_tree = Node(fsf, 9, sfs);;

count' (fun x -> x = 5) ex_tree;; (* 3 *)
count' (fun x -> x = 7) ex_tree;; (* 3 *)
count' (fun x -> x mod 2 = 1) ex_tree;; (* 7 *)

(* The very general type affords us many benefits, tail recursion, and
   versatility). Consider we are not interested in how many results we
   have, but just if there are too many of them (we all now that more
   thatn 5 results is too many) *)


(* We can implement this function by taking advantage of the
   continuation. For the observant student, this function will be less
   efficient than what it technically could because it always counts
   all the results instead of stopping after 5 *)
let too_many p t = count_k p t (fun n -> n > 5)

(* The mapping function on lists can be also generalized to other data
   structures, like trees. In this example we write that function for
   trees. *)
let rec map_tree (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Leaf -> Leaf
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

(* As before, many functions on trees are difficult to write in a tail
   recursive way without continuations. However, continuations make it
   easy, let's write map with continuations. Again, consider the type
   of the continuation passing version. *)

let rec map_tree_k (f : 'a -> 'b) (t : 'a tree) (k : 'b tree -> 'c) : 'c =
  match t with
  | Leaf -> k (Leaf)
  (* l' waits on result from l, r' waits on result from r, call f x on val of node  *)
  | Node (l, x, r) -> map_tree_k f l (fun l' -> map_tree_k f r (fun r' -> k (Node(l', (f x), r'))))

(* Finally, write a map_tree' that behaves like map_tree but it is
   tail recursive and uses map_tree_k *)
let map_tree' f t = map_tree_k f t (fun x -> x)

let doub_five = map_tree' (fun x -> 2*x) five;;
let doub_sfs = map_tree' (fun x -> 2*x) sfs;;
let double_ex = map_tree' (fun x -> 2*x) ex_tree;;

(*  Intuitively, fold_right replaces every :: by f and nil
  by e in a list. The function tree_fold for binary trees is analogous to 
  fold_right. Given a tree, tree_fold replaces each leaf by some 
  value e and each node by the application of a 3-argument function  f 

  It has type:

  tree_fold: ('a * 'b * 'b -> 'b) -> 'b -> 'a tree -> 'b 

  Example: Given a tree 
  Node (x0, Node (Leaf, x1, Leaf), 
            Node (Node (Leaf, x3, Leaf), x2, Leaf)) 

  the result will be 

  f (f (e, x1, e), x0,  
     f (f ( e, x3, e), x2, e))

 *)

let rec tree_fold f e t = match t with
  | Leaf -> e (* Return base *)
  (* Call tree_fold on each side and the val *)
  | Node(l, x, r) -> f ((tree_fold f e l), x, (tree_fold f e r));;


(* The tree_fold function allows us to express many programs which 
   traverse trees elegantly in one line.

  a) Re-implement the function size : 'a tree -> int which given a
   binary tree returns the number of nodes in the tree using tree_fold
   (5 points)

  b) Implement the function reflect : 'a tree -> int which given a
   binary tree swaps the left and the right child using tree_fold

  c) Implement inorder: 'a tree -> 'a list which given a binary tree
   returns a list of all entries in order.

 *)
(* Add 1 for every value we have, base of l and r eventually hit 0 *)
let size tr = tree_fold (fun (l, x, r) -> l + 1 + r) 0 tr;;
size ex_tree;;

let reflect tr = tree_fold (fun (l, x, r) -> Node(r, x, l)) Leaf tr;;
reflect ex_tree;;

let inorder tr = tree_fold (fun (l, x, r) -> l @ [x] @ r) [] tr;;
inorder ex_tree;;


(* More on using failure continuations .... *)
(* Remember how to give change using exceptions?  ...
   well, we can use continuations instead of exceptions! 
 *)
exception Change

(* change: : int list -> int -> int list *)
let rec change coins amt = 
  if amt = 0 then [] 
  else 
    begin match coins with 
    | [] -> raise Change
    | coin::cs ->  
       if coin > amt then
	 change cs amt
       else 
	 try 
	   coin :: change coins (amt - coin)
	 with Change -> change cs amt
    end
  
let rec listToString = function
  | [] -> ""
  (* Don't end with a comma *)
  | h::[] -> (string_of_int h)
  | h::t -> (string_of_int h)^","^(listToString t)

(* Tail recursive *)
let rec listToString' l =
  let rec aux l acc =
    match l with
    | [] -> acc
    (* Don't end with a comma *)
    | h::[] -> (aux [] (acc ^ string_of_int (h)))
    | h::t -> aux t (acc ^ string_of_int (h) ^ ",") in
  aux l ""


let change_top coins amt =
  try 
    let c = change coins amt in
    print_string ("Return the following change: " ^ listToString c ^ "\n")
  with Change -> print_string ("Sorry, I cannot give change\n")


(* Giving change with continuations *)
let rec cchange (coins: int list) (amt:int) (fc: unit -> int list) =
  (* Add a success continuation as well *)
  let rec aux coins amt fc sc = 
    if amt = 0 then sc ()
    else 
      begin match coins with
      | [] -> fc ()
      | coin::cs ->
         if coin > amt then
           aux cs amt fc sc
         else
           aux coins (amt - coin) (fun () -> aux cs amt fc sc) (fun () -> (sc ()) @ [coin])
      end
                                                                in aux coins amt fc (fun () -> []);;

let cchange_top coins amt = 
  try 
    let c = cchange coins amt (fun () -> raise Change) in
    print_string ("Return the following change: " ^ listToString c ^ "\n")
  with Change -> print_string ("Sorry, I cannot give change\n");;

cchange_top [2;5] 3;;
cchange_top [2;5] 8;;
cchange_top [25;10;5;2] 43;;
change_top [5;3;2] 9;;
cchange_top [5;3;2] 9;;

(* Here is the behavior of change_top : 

# change_top [2;5] 3;;
Sorry, I cannot give change
- : unit = ()
# change_top [2;5] 8;;
Return the following change: 2, 2, 2, 2
- : unit = ()
# change_top [25;10;5;2] 43;;
Return the following change: 25, 10, 2, 2, 2, 2
- : unit = ()
# change_top [25;10;5;2] 44;;
Return the following change: 25, 10, 5, 2, 2
- : unit = ()
# change_top [25;10;2] 44;;
Return the following change: 10, 10, 10, 10, 2, 2
- : unit = ()
# change_top [25;10;2] 43;;
Return the following change: 25, 10, 2, 2, 2, 2
- : unit = ()
# change_top [25;10;2] 23;;
Sorry, I cannot give change
- : unit = ()
# 
 *)
(* ---------------------------------------------------- *)
(* Lazy Programming                                     *)
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)
(* Suspended computation : we can suspend computation
   by wrapping it in a closure. *)
type 'a susp = Susp of (unit -> 'a)

(* delay: *)
                                                                                             let delay f = Susp(f)

(* force: *)
let force (Susp f) = f ()

(* ---------------------------------------------------- *)
(* Define an infinite stream via observations we can make 
                                                         about it.
 *)

type 'a str = {hd: 'a  ; tl : ('a str) susp} 

let rec str_ones = {hd = 1 ; tl = Susp (fun () -> str_ones)}

let rec numsFrom n = 
  {hd = n ; 
   tl = Susp (fun () -> numsFrom (n+1))}

let nats = numsFrom 0

(* ---------------------------------------------------- *)
(* Inspect a stream up to n elements 

 *)
let rec take_str n s = match n with 
  | 0 -> []
  | n -> s.hd :: take_str (n-1) (force s.tl)

(* ------------------------------------------------------- *)
(* val zip : ('a -> 'b -> 'c) -> 'a str -> 'b str -> 'c str *)
(* zip two streams togethether using f *)
let rec zip f s1 s2 =
  {hd = f s1.hd s2.hd ;
   tl = Susp (fun () -> zip f (force(s1.tl)) (force(s2.tl)))
  }


let rec add_str s1 s2 = zip (fun x1 x2 -> x1 + x2) s1 s2;;

let double = add_str (nats) (nats);;

take_str 10 double;;

(* ------------------------------------------------------- *)
(* Computing partial sums lazily over a stream of nats   
                                                    
                                                    psums 0 1 2 3 4 5  ..... 
                                                    returns: 0 1 3 6 10 15 ...

 *)

let rec psums s =
  let s' = force(s.tl) in
  {hd = s.hd ;
   tl = Susp (fun () -> add_str (psums s) s')
  }
  
let pSeq = psums nats;;
take_str 10 pSeq;;

(* ---------------------------------------------------- *)
(* Objects and references                               *)
(* ---------------------------------------------------- *)

type action = Open | Close
exception Error of string 

(* Write a function make_lock  generates a function  action -> unit
   which acts on a lock which is either Open or Close. 

   If the lock is Open and ask to Close it, we set the lock to Close.
   If the lock is Close and we ask to Open it, we set the lock to Open.
   If the lock is Open and we ask to Open it 
   or the lock is Close and we ask to Close it (again) then we raise an error.
   
 *)
let make_lock  =
  let lock = ref Open in
  function
  | Close -> if !lock = Open then lock := Close else raise (Failure "Can't set closed lock closed")
  | Open -> if !lock = Close then lock := Open else raise (Failure "Can't set open lock opened")

let l = make_lock;;
l Close;;
l Open;;
(* l Open;; (\* Exception *\) *)
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)
(* INDUCTION PROOF *)

let rec size t = match t with 
  | Leaf -> 0 
  | Node (l, x, r) -> x + size l + size r

let rec size_acc t acc = match t with 
  | Leaf -> acc
  | Node (l, x, r) -> size_acc l (x + size_acc  r acc)  

                               (*
THEOREM:  size t + acc = size_acc t acc 

                                *)
                               (* ---------------------------------------------------- *)
                               (* THANK YOU - IT WAS A LOT OF FUN!                     *)
                               (* ---------------------------------------------------- *)

