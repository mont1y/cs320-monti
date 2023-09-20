#use "./../../../classlib/OCaml/MyOCaml.ml";; 

let strcat a b =
  let len = string_length a + string_length b in
  string_init len (fun i ->
    if i < string_length a then
      string_get_at(a)(i)
    else
      string_get_at(b)(i - string_length a)
)

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


let string_longest_ascend(xs: string): string =
  (* curLong is the current longest consecutive string
     count is the count of the length of the current consecutive string 
     i is the index on the string*)
  let rec helper finLong newLong i =
    if (i < string_length xs) then
      let rec helper2 (curLong: string) (j:int): string =
        if (j < string_length xs) then
          (* A recursive function that's basically minimax
          what I need a minimax in here? that's insane!!
          let rec helper3 count index () *)
          if (string_length curLong = 0) then
            helper2 (string_cons(string_get_at(xs)(j)) (curLong)) (j+1)
          else if (string_get_at(xs)(j) >= string_get_at(curLong)(0)) then
            helper2 (string_cons(string_get_at(xs)(j)) (curLong)) (j+1)
          else
            helper2 curLong (j+1)
        else
          curLong
      in
      let newLong = helper2 "" i in
      if (string_length finLong < string_length newLong) then
        helper newLong "" (i+1)
      else
        helper finLong "" (i+1)
    else
      stringrev(finLong)
  in
  helper "" "" 0
;;