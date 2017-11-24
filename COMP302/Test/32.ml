(* Functions that demonstrates that typechecking can be exponential 
   in the worse case  *)
let id x = x
let f y z = z y y
let g y = f (f y)
let h y = g (g y)
let k y = h (h y)
;;
(* let l y = k (k y) *)
f;;
g;;
h;;
k;;
(* Why are these types growing so much? *)
(* l grows A LOT  *)
