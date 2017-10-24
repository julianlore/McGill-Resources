let listToString l = match l with
  | [] -> ""
  | l ->
     let rec toString l = match l with
       | [h] -> string_of_int h
       | h::t -> string_of_int h ^ ", " ^ toString t
     in
     toString l

              (*        change [50;25;10;5;2;1] 43;;
               * [25; 10; 5; 2; 1]
               *   change [50;25;10;5;2;1] 13;;
 * [10;2;1]
 *   change [5;2;1] 13;;
 * [5;5;2;1] *)

exception Change

let rec change coins amt =
  if amt = 0 then []
  else
    (
      match coins with
      | [] -> raise Change
      (* Cannot print here, as it won't do any backtracking and 
       * will also give you a type error since it's a unit and we're trying to build a list *)
      (* raise Change is any type  *)
      | coin :: cs -> if coin > amt then change cs amt
                      else (* coin <= amt*)
                        (* Prepend coin as we're trying to use this coin for result *)
                        try coin::(change coins (amt - coin) )
                                    (* Try, if you fail, try again without given coins*)
                        with Change -> change cs amt
    )
