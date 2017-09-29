let rec fact_tr m =
  let rec f(n,m) =
    if n=0 then
      m
    else f(n-1,n*m)
  in
  f(n,1)
