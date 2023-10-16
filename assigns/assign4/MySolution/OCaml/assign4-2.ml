#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let theNatPairs: (int * int) stream = fun () ->
  let rec generate_pairs i j = fun () ->
    StrCons((i, j), fun () ->
      if i = 0 then
        generate_pairs(j + 1)(i) ()
      else
        generate_pairs(i - 1)(j+1) ()
    )
  in
  (generate_pairs (0)(0))  ()
;;