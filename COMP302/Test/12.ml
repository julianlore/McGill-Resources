(* simplified roulette *)
type colour = Red | Black

type result = colour option (* Result of run*)

type amt = int
type bet = amt * colour

type id = string
type player = id * amt * bet option

(* See who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(* same as: *)

(* let compute (am, col) r = match r with
 *   | None -> 0
 *   | Some col' -> if col = col' then am * 2 else 0 *)

(* Solve all these questions without using recursion or 
 * pattern matching on lists, but instead just use the HO functions we saw in class *)

let bets = [ ("Aliya", 1000, Some (400, Red)) ;
             ("Jerome", 800, Some (240, Black)) ;
             ("Mo", 900, Some (200, Black)) ;
             ("Andrea", 950, Some (100, Red))]

(* Q1: given a list of players compute the new amounts each player has and set their bets to None *)
let compute_all_results (l : player list) (r : result) =
  (* Should map players to their new vals, player has name id and amt. 
   * What function? Act on bet type
   * Keep id, add compute to amount and no more bet 
   * Need to get bet out of bet option
   * Use pattern matching*)
  
  (* List.map (fun (id, amt, bopt) -> match bopt with
   *                                  | None -> (id, amt, bopt)
   *                                  | Some b -> (id, amt + compute b r , None)) l *)

  (* Alternative with function *)
  List.map (function (id, amt, Some b) -> (id, amt +compute b r , None)
                   | (id, amt, None) -> (id, amt, None)) l
;;
compute_all_results bets (Some Red);;

(* Q2: given a list of bets and a result 
compute a list of winning players with their bets *)

(* Use filter *)
let compute_winners (l : player list) (r : result) =
  List.filter(function (id, amt, Some b) -> compute b r > 0
                     | (id, amt, None) -> false) l
;;
compute_winners bets (Some Red);;

(* Q3: given a list of bets and a result compute 
 * how much money the casino needs to pay back*)

(* Use fold *)

(* Q4 : given a list of bets and a result 
 * compute if nobody won *)
(* Check if there is a winner (exists ho func) or if everyone is a loser (for_all) *)
