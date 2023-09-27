#use "./../../../classlib/OCaml/MyOCaml.ml";; 
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