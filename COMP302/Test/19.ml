(* Want to give an interface to this module 
 * What functions do we want to expose? 
 * Called signature in OCaml*)
module type STACK =
  sig
    type stack
    type el
    val empty : unit -> stack
    val is_empty : stack -> bool
    val pop : stack -> stack option
    val push : el -> stack -> stack
(* val push : int -> stack -> stack *)
  (* If we change this to el -> stack -> stack 
 * Stack.push 1 s wouldn't work, because 1 isn't a Stack.el*)
  end

(* 1 program unit with namespace separation *)
(* If you want to access the module need to write Stack.function *)

(* module Stack = *)

(* Specify that Stack implements STACK *)
(* module Stack : STACK = *)

(* Specify what type el is *)
module Stack : (STACK with type el = int) =
  struct
    type el = int
    type stack = int list

    let empty () : stack = []

    let push i (s : stack) = i::s

    let is_empty s = match s with
      | [] -> true
      | _::_ -> false

    let pop s = match s with
      | [] -> None
      | _::t -> Some t

    let top s = match s with
      | [] -> None
      | h::_ -> Some h

    let rec length s acc = match s with
      | [] -> acc
      | x::t -> length t 1+acc

    let size s = length s 0

    let stack2list(s:stack) = s
  end

let s = Stack.empty();;
(* empty;; -> unbound, packaged in module *)
let s1 = Stack.push 1 s;;
(* Will not show you that the stack is a list 
 * Didn't specify in the signature *)
(* Cannot do: 1 :: s1;; *)
(* By tying module to signature, you are hiding information *)

module FloatStack : (STACK with type el = float) =
  struct
    type el = float
    type stack = float list

    let empty () : stack = []

    let push i (s : stack) = i::s

    let is_empty s = match s with
      | [] -> true
      | _::_ -> false

    let pop s = match s with
      | [] -> None
      | _::t -> Some t

    let top s = match s with
      | [] -> None
      | h::_ -> Some h

    let rec length s acc = match s with
      | [] -> acc
      | x::t -> length t 1+acc

    let size s = length s 0

    let stack2list(s:stack) = s
  end

module IS = Stack
module FS = FloatStack

(* How do we test the length function without a module specifying it? 
 * DANGEROUS
 * Use open*)
(* open Stack;; 
 * Now erases all namespace boundaries
 * Don't need to prefix anymore *)
