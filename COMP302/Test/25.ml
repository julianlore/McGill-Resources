(* Continuation of last class *)
#use "24.ml";;

(* Generate: 1, 1/2, 1/4, 1/8, 1/16 *)
(* geom_series: float -> float str *)
let rec geom_series x =
  {hd = (1.0 /. x) ;
   tl = Susp (fun () -> geom_series (x *. 2.0))
  };;

take 10 (geom_series 1.0);;

(* zip: ('a * 'b -> 'c) -> 'a str -> 'b str -> 'c str *)
let rec zip f s1 s2 =
  {hd = f (s1.hd,s2.hd) ; 
   tl = Susp (fun () -> zip f (force s1.tl) (force s2.tl))
  }

(* sfilter: ('a -> bool) -> 'a str -> 'a str *)
let rec sfilter f s =
  let h,t = find_hd f s in
  {hd = h ;
   tl = Susp (fun () -> sfilter f (force t))
  }

(* find_hd: ('a bool) -> 'a str -> 'a * ('a str) susp *)
(* Use to find first element in stream satisfying f, return with tail     *)
and find_hd f s =
  if (f s.hd) then (s.hd, s.tl) else find_hd f (force s.tl);;

(* All even numbers *)
take 10 (sfilter (fun x -> x mod 2 = 0) nats);;

(* Will filter always be productive? No. It may never be true in a stream 
 * Your responsibility now *)

(* Sieve of Eratosthenes for prime numbers *)
(* Start with natural numbers from 2 *)
(* Take head and filter out all elements divisible by head *)
(* Repeat *)
let rec sieve s =
  {hd = s.hd ;
   tl = Susp(fun() -> sfilter (fun x -> not (x mod s.hd = 0)) (sieve (force s.tl)))
  }
let nats2 = numsFrom 2
let primes = sieve nats2;;

take 10 primes;;
(* take 1000 primes;; *)

(* fib(n+1)=fib(n)+fib(n-1) *)
(* fibs 0 1 1 2 3 5 *)
(* Make a copy of this but shifted *)
(* Add both entries to get next fib number *)
(* Essentially adding two streams *)
let rec fibs =
  {hd = 0 ;
   tl = Susp (fun () -> fibs')
  }
and fibs' =
  {hd = 1 ;
   tl = Susp (fun () -> add fibs fibs')};;

take 10 fibs;;
