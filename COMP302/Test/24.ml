type 'a susp = Susp of (unit -> 'a)
(* Delay computation *)

(* Force computation *)
let force (Susp f) = f ()

type 'a str =
  {hd : 'a ; tl : ('a str) susp}

    (* Stream of 1, 1, 1, 1, 1 .... *)
    (* ones : int str *)

let rec ones =
  {hd = 1 ;
   tl = Susp (fun () -> ones) (* Suspend comp to generate more ones *)
  }
  
(* numsFrom n  generates stream n, n+1, n+2, ... *)
let rec numsFrom n =
  {hd = n ;
   tl = Susp (fun () -> numsFrom (n+1))} (* Ocaml stops evaluating at function *)

let nats = numsFrom 0

(* series starting with n and i-th element is k^i *)
let rec pow_seq n k =
  {hd = n ;
   tl = Susp (fun () -> pow_seq (n*k) (k))}

(* add: int str -> int str -> int str *)
let rec add s1 s2 =
  {hd = s1.hd + s2.hd;
   (* tl = Susp (fun() -> add s1.tl s2.tl)  -> this gives you an error, need to force for elm *)
   tl = Susp (fun() -> add (force s1.tl) (force s2.tl))
  }

(* smap: ('a -> 'b) -> 'a str -> 'b str *)
let rec smap f s =
  {hd = f (s.hd) ;
   tl = Susp (fun () -> smap f (force s.tl))
  }
(* take: int -> 'a str -> 'a list *)
(* peels off n elements from stream *)
let rec take n s = if n = 0 then []
                   else s.hd :: take (n-1) (force s.tl);;

take 10 ones;;
take 10 (numsFrom 1);;
take 10 (pow_seq 1 2);;
take 10 (add nats ones);;
take 10 (add nats nats);;
take 10 (smap (fun x -> x*2) nats);;
