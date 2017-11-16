#use "27.ml"

let rec member x l = List.exists (fun y -> y = x) l

let rec delete (vlist, l) = match l with
  | [] -> []
  | h :: t ->
     if member h vlist then delete (vlist, t)
     else h::delete(vlist,t)

(* let rec union p = match p with *)

let rec freeVars e = match e with
  | Int _ -> []
  | Bool _ -> []
  | If (e, e1, e2) -> union (freeVars e, union (freeVars e1, freeVars e2))
  | Let (Val (e1, x), e2) -> union (freeVars e1, delete([x], freeVars e2))
  | Primop (po, args) ->
     List.fold_right (fun e fv -> union (fv, freeVars e)) args []
