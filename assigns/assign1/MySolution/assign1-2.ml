
#use "./../../../classlib/OCaml/MyOCaml.ml";; 
let string_merge(cs1) (cs2) =
  let rec helper i j work =
    if string_length cs1=i then
      int1_foreach(string_length cs2 - j)(fun x -> work(string_get_at(cs2)(j + x)))
    else if string_length cs2=j then
      int1_foreach(string_length cs1 - i)(fun x -> work(string_get_at(cs1) (i + x)))
    else if ord (string_get_at(cs1) (i)) < ord (string_get_at(cs2) (j)) then
      (work(string_get_at(cs1) (i)); helper(i+1)(j)work)
    else
      (work(string_get_at(cs2) (j)); helper(i)(j+1)work)
  in
  string_make_fwork(helper 0 0)
;;

let () = Printf.printf "stringmerge 239 acd: %s\n%!" (string_merge("135")("2468"));;