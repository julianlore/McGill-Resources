(* Defining a new language in OCaml *)
module Exp =
  struct
    type name = string
    type primop = Equals | LessThan | Plus | Minus | Times | Negate

    type exp =
      | Var of name (* From Lecture 28 *)
      | Int of int (* 0 | 1 | 2 | ... *)
      | Bool of bool (* true | false *)
      | If of exp * exp * exp (* if e then e_1 else e_2 *)
      | Primop of primop * exp list (*e_1 primop e_2 or primop e *)
      | Let of dec * exp (* From L28 let dec in e end *)

    and dec = (* From L28 *)
      | Val of exp * name (* val x =e *) 

    (* How to evaluate? *)
    module Eval =
      struct
        open exp

        exception Stuck of string

        let evalOp op = match op with
          | (Equals, [Int i; Int i']) -> Some (Bool (i = i'))
          | (LessThan, [Int i; Int i']) -> Some (Bool (i < i'))
          | (Plus, [Int i; Int i']) -> Some (Int (i + i'))
          | (Minus, [Int i; Int i']) -> Some (Int (i - i'))
          | (Times, [Int i; Int i']) -> Some (Int (i * i'))
          | (Negate, [Int i]) -> Some (Int (-i))
          | _ -> None
               
        let rec eval e = match e with
          | Int _ -> e
          | Bool _ -> e
          | If (e, e1, e2) ->
             (match eval e with
              | Bool true -> eval e1
              | Bool false -> eval e2
              | _ -> raise (Stuck "guard is not  bool"))
          (* Primitive operations *)
          | Primop (po, args) ->
             let argvalues = List.map eval args in
             (match evalOp (po, argvalues) with
              | Some v -> v
              | None -> raise (Stuck "wrong arguments passed to prim. op"))
             
      end
        (* The rest is on course page *)
