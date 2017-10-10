(* Part 1: define a function that computes the parity of a list of
   booleans.

   What we want is a function even_parity such as even_parity l
   returns a boolean that combined with the elements of l the 
   number of 1’s (true) is even.
   This is a simple but important algorithm in data communication, its
   purpose is to detect errors (one bit flips). It is very easy to
   implement in circuit but it is not very reliable.
 *)

let rec even_parity = function
  | [] -> false
  | true::xs -> not (even_parity xs)
  | false::xs -> even_parity xs

(* This version is very natural but it is not tail recursive. Let's
   make the tail recursive version. *)

let even_parity_tr l =
  let rec parity p = function
    | [] -> p
    | p'::xs -> parity (p<>p') xs
  in
  parity false l

(* Part 2: Now prove that both functions are equivalent. You will need to use facts about the <> (XOR) operation *)

let ex_1 = [true]
let ex_2 = [true; true]
let ex_3 = [false; true; true; false; true]

let t_1 = even_parity ex_1 = even_parity_tr ex_1
let t_2 = even_parity ex_2 = even_parity_tr ex_2
let t_3 = even_parity ex_3 = even_parity_tr ex_3

(* High order - croupier for simplified roulette *)

(* We have a simplified roulette, where we have only two colours that
   we can bet but if zero comes out, everyone loses *)

type colour = Red | Black        (* The two colours we can bet on *)

type result = colour option      (* The result of a run, it could be one of the colours or no colour if zero came up *)

type bet = int * colour          (* The bet amount and to what colour *)


(* It is simple to see who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(*
Solve all these questions without using recursion or pattern
matching on lists, but instead just use the HO functions we saw
in class.
 *)

let bets = [(1000, Red); (300, Black); (400, Red)]
(* let bets = [(1000, Red); (2000, Black)] *)
(* Q1:  given a list of bets compute the results *) 
(* let x = compute (1000, Red) (Some Red) *)
let compute_results (b : bet list) (r : result) =
  List.map (fun (amt, col) -> (compute (amt, col) r, col)) b

  
let q1 = compute_results bets (Some Red)
(* Q2: given a list of bets and a result compute a list of winning bets *)
let get_winners (b : bet list) (r : result) =
  List.filter (fun (amt, col) -> compute (amt, col) r > 0) b

let get_winners' (b : bet list) (r : result) =
  List.filter (fun (amt, col) -> Some col = r) b
  
let q2 = get_winners bets (Some Red)
let q2' = get_winners' bets (Some Red)

(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)

let get_payback (b : bet list) (r : result) =
  List.fold_right (fun (amt, col) (sum : int) -> (sum + compute (amt,col) r)) b 0

(* or using List.fold_left *)
let get_payback' (b : bet list) (r : result) =
  List.fold_left (fun (sum : int) (amt, col) -> (sum + compute (amt, col) r)) 0 b
  
let q3 = get_payback bets (Some Red)
let q3' = get_payback bets (Some Red)
(* Q4: given a list of bets and a result compute if everyone won *)
let everyone_won (b : bet list) (r : result) =
  List.for_all (fun (amt, col) -> compute (amt, col) r > 0) b
  
(* let bets = [(1000, Red) ; (100, Red)] *)
  
let q4 = everyone_won bets (Some Red)

(* Q5: given a list of bets and a result compute if someone won *)
let someone_won (b : bet list) (r : result) =
  List.exists (fun (amt, col) -> compute (amt, col) r > 0) b

(* let bets = [(1000, Black)] *)
  
let q5 = someone_won bets (Some Red) 

(* Q6: given a list of bets return the highest winning *)
let highest_win (b : bet list) (r : result) = 
  List.fold_right (fun (amt, col) (max : int) ->
      let win = compute (amt,col) r in
      if win > max then win
      else max) b 0

let q6 = highest_win bets (Some Red)

(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)
let balance (b : bet list) (r : result) =
  let loss = get_payback b r in (* Use q3*)
  let losers = List.filter(fun (amt, col) -> compute (amt, col) r = 0) b in
  List.fold_right (fun (amt, col) (sum : int) -> sum + amt) losers (-loss)

let q7 = balance bets (Some Red)

(* Ninja level  *)

(* Q8: Can you sort the results by the amount they made? *)

let sort (b : bet list) (r : result) =
  let resulting_bets = compute_results b r in (* Q1 *)
  List.sort (fun (amt, col) (amt', col') -> compare amt amt') resulting_bets

let q8 = sort bets (Some Red)
