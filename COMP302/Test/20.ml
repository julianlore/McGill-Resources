module type CURRENCY =
  sig
    type t
    val unit : t
    val plus : t -> t -> t
    val prod : float -> t -> t
    val toString : t -> string
  end;;

(* Here, float is not yet tied to currency *)
(* Ideally, we'd also like to define multiple currencies *)
module Float =
  struct
    type t = float
    let unit = 1.0
    let plus = (+.)
    let prod = ( *. )
    let toString x = string_of_float x
  end;;

(* Abbreviation for a module *)
(* module Euro = Float *)
(* But we don't just want to abbreviate it, 
 * want to also say it implements currency *)
module Euro = (Float : CURRENCY);;
module USD = (Float : CURRENCY);;
module CAD = (Float : CURRENCY);;
module BitCoins = (Float : CURRENCY);;
(* All these currencies use the same implementation of float, 
 * but all referring to different implementations 
 * Important that we specify CURRENCY here rather than in float
 * so that they can be different
 * Want to keep abstraction. We made plus and prod require Euros
 * so we don't just add floats*)
(* Isomorphic structures, but all accessed differently *)

(* Conversion functions  *)
let euro x = Euro.prod x Euro.unit
let usd x = USD.prod x USD.unit
let cad x = CAD.prod x CAD.unit
let bitcoins x = BitCoins.prod x BitCoins.unit

let x = Euro.plus (euro 10.0) (euro 20.5);;
(* Will not show result because it is abstract 
 * Can show result by printing/showing with toString*)

Euro.toString x;;

(* Euro.plus (euro 10.0) (10.0) does not work *)

Euro.plus;;
(* Euro.t -> Euro.t -> Euro.t 
 * Requires Euros *)

(* If we say that Float : CURRENCY when declaring module 
 * will still get different types for 
 * module Euro = Float;; module USD = Float;;
 * Just aliases for different types
 * Not actually different!
 * Binding to signature multiple times makes them all different
 * Isomorphic, can do the same stuff, but not the same type
 * But then you'll be able to add USDs and EUROs*)

(* Important principle since you're abstracting 
 * implementations but also not mixing currencies *)

(* Now let's think of banks and their view 
   and how we'll implement it for them *)
module type CLIENT = (* Client's view*)
  sig
    type t (* account *)
    type currency
    val deposit : t -> currency -> currency
    val retrieve : t -> currency -> currency
    val print_balance: t -> string
  end;;

module type BANK =
  sig
    include CLIENT (* Inheritance *)
(* Don't have to literally copy all things from the client module 
 * Now has the same things as client*)

    val create : unit -> t

  end;;

(* We want banks of different currencies 
 * with particular functions, adding 2 currencies
 * printing currencies, etc.*)

(* Parameterize a module Old_Bank with the functionality 
   provided by the module type CURRENCY *)
(* Should not matter what type of currency, 
   bank should be able to do the same thing regardless of currency *)

(* Module parametized by another module is also called a functor 
 * Similar to higher order functions but for modules *)
module Old_Bank (M : CURRENCY) : (BANK with type currency = M.t) =
  (* M describes a module, can implement a module 
     with a module for currency, M *)
  struct
    type currency = M.t
    type t = { mutable balance : currency }
    (* Could have made it a currency ref *)
           
    let zero = M.prod 0.0 M.unit
    and neg = M.prod (-1.0)

    let create() = { balance = zero }

    let deposit c x =
      if x > zero then
        c.balance <- M.plus c.balance x; c.balance

    let retrieve c x =
      if c.balance > x then
        deposit c (neg x)
      else
        c.balance

    let print_balance c =
      M.toString c.balance
  end;;

(* How do we get an implementation of a bank now? *)

module Post = Old_Bank (Euro);;
(* How to make the client see less? *)
module Post_Client : (CLIENT with type currency = Post.currency and type t = Post.t) = Post;;
(* Tells you what is shared with Post *)

let my_account = Post.create () ;;
Post.deposit my_account (euro 100.0);;
Post_Client.deposit my_account (euro 10.00);;
Post.print_balance my_account;;
Post_Client.print_balance my_account;;
(* Shared functionality among the two, but different ways of accessing *)

module Citybank = Old_Bank (USD);;
module Citybank_Client : (CLIENT with type currency = Citybank.currency and type t = Citybank.t) = Citybank;;

let my_cb_account = Citybank.create ();;
Citybank.deposit my_cb_account (usd 50.00);;
(* Citybank_Client.deposit my_account;; Won't work *)
