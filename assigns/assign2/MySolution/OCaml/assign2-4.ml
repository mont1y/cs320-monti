#use "./../../../../classlib/OCaml/MyOCaml.ml";; 

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
      let _ = foldleft(xs)(0)(fun (r)(x) -> (work(r)(x);r+1))
  in
  ();;
let
list_iforeach =
fun xs -> foldleft_to_iforeach(list_foldleft)(xs)
;;

let
string_sepjoin_list
(sep: string)(xs: string list): string =
 string_make_fwork(
  fun work ->
    list_iforeach xs (fun i x -> (if i > 0 then (string_foreach sep work)); string_foreach x work)
  )
  ;;
  (* ****** ****** *)