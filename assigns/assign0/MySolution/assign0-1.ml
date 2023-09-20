#use "./../assign0.ml";;

let rec fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1;;

let rec
myans(x:int): int =
if fact(x) = 0 then x else myans(x+1)
;;