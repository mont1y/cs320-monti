#use "./../../../classlib/OCaml/MyOCaml.ml";; 
let strcat a b =
  let len = string_length a + string_length b in
  string_init len (fun i ->
    if i < string_length a then
      string_get_at(a)(i)
    else
      string_get_at(b)(i - string_length a)
)


let int2str(i0: int): string = 
  let rec getEach n conc=
    if (n < 10) then
      (* Add Digit to String *)
      strcat (str(chr (n + ord '0'))) conc
    else
      let num = n mod 10 in
      let new_conc = strcat (str (chr (num + ord '0'))) conc in
      (* Add this char to String *)
      getEach (n / 10) new_conc
  in
  if i0 = 0 then "0" 
  else if i0 < 0 then strcat "-"  (getEach (abs i0) "")
  else getEach i0 "";;

let stringrev(cs: string): string = 
(* i is the index, starting from 0 *)
let rec helprev revstr i =
  if i >= (string_length cs) then
    revstr
  else
    let new_revstr = strcat (str(string_get_at(cs)(i))) revstr in
    helprev new_revstr (i+1)
in
helprev "" 0;;


(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)

(* ****** ****** *)
let countdig(n:int): int = 
  let intstr = int2str(n) in
  string_length intstr;;

let () = Printf.printf "numdig in 239: %i\n%!" (countdig(2314389));;

(* let getdig num index =
  let rec gdig  *)

let intrev10(n: int): int = 
  (* return (str2int(rev(str n))) *)
  let rec getEach cs itr i mul =
    if itr <= 0 then
      i
    else
      if (string_get_at(cs)(itr - 1)) = '-' then
        i
      else 
        let new_i = i + (((ord (string_get_at(cs)(itr - 1))) - ord '0') * mul) in
        getEach cs (itr - 1) new_i (mul*10)
  in
    getEach (stringrev(int2str n)) (string_length (stringrev(int2str n))) 0 1

let () = Printf.printf "revint 239: %i\n%!" (intrev10(789));;