#use "./../assign0.ml";;

let 
isPrime(x:int): bool =
  let rec loopy(n:int): bool = 
    if n * n > x then true
    else if x mod n = 0 then false
    else loopy(n+1)
  in
  if x < 2 then false 
  else if x == 2 then true
  else if loopy(2) = true then true
  else false
;;