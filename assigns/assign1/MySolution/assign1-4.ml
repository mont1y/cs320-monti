#use "./../../../classlib/OCaml/MyOCaml.ml";; 
let pad (numzero:int) (str:string): string =
  let len = string_length str + numzero in
  string_init len (fun i ->
    if i < numzero then
      '0'
    else
      string_get_at(str)(i - numzero)
)

let intrep_add(ds1: string)(ds2: string): string =
  (* i is index, c is carry digit *)
  let rec add i c newstr newds1 newds2=
    if (i >= 0) then
      let sum = (ord (string_get_at(newds1)(i)) - ord('0')) + (ord (string_get_at(newds2)(i)) - ord('0'))
      in
      if sum >= 10 || (sum = 9 && c = 1) then
        add (i-1) (1) (string_cons(char_of_digit(sum-10 + c)) newstr) newds1 newds2
      else
        add (i-1) (0) (string_cons(char_of_digit(sum + c)) newstr) newds1 newds2
    else
      if (c = 1) then
        (string_cons('1') newstr)
      else
        newstr
  in
  if (string_length ds1 >= string_length ds2) then
    add(string_length ds1 - 1)(0)("") ds1 (pad(string_length ds1 - string_length ds2)(ds2))
  else
    add(string_length ds2 - 1)(0)("") (pad(string_length ds2 - string_length ds1)(ds1)) ds2
;;

let () = Printf.printf "intrep_att: %s\n%!" (intrep_add "1" "18");;
let () = Printf.printf "turn 3 to digit: %i\n%!" (ord ('3') - ord('0'));;