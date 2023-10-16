#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let the_ln2_stream: float stream = fun() ->
  let rec streamize(denom)(sum)(sign): float stream = fun() ->
    let nextNum = float(sign) /. denom in
    let newSum = sum +. nextNum in
    StrCons(newSum, streamize(denom+.1.0)(newSum)(sign * -1))
  in
  (streamize 1.0 0.0 (1)) ()
;;