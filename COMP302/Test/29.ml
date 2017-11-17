#use "28.ml"

let rec subst ((e',x) as s) e = match e with
  | Int _ -> e
  | Bool _ -> e
  | Var y -> if x = y then e' else e
  | If (e0, e1, e2) -> If (subst s e0, subst s e subst s e2)
  | Primop (po, args) -> (Primo (po, List.map subst s) args)
  | Let (Val (e1, y), e2) ->
     if x = y || member y (freeVars e') then
       let y1' = freshVar y in
       (* Ex.  [y*5/x] (let a = x + 1 in x + a)*)
       (* 1. Rename y with a in (x+y) 
          Where a is a fresh variable*)
       let x2' = rename (Var (y') , y) e2 in
       
     else Let (Val (subst s e1, y))
    
and rename r e = subst r e
