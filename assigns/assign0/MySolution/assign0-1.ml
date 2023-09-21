#use "./../assign0.ml";;

let rec fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1;;

let rec
loop(x:int): int =
if fact(x) = 0 then x else loop(x+1)
;;

let myans = loop(0);;