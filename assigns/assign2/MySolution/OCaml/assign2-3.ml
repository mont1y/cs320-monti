#use "./../../../classlib/OCaml/MyOCaml.ml";; 
(* ****** ****** *)
let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;;
(* ****** ****** *)

let foldleft_to_iforeach(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  fun(xs)(work) -> 
      let _ = foldleft(xs)(0)(fun (i)(x) -> (work(i)(x);i+1))
  in
  ();;