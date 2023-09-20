#use "./../assign0.ml";;


let strcat a b =
  let len = string_length a + string_length b in
  string_init len (fun i ->
    if i < string_length a then
      string_get_at(a)(i)
    else
      string_get_at(b)(i - string_length a)
    ;;


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