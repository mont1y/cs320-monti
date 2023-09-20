#use "./../assign0.ml";;

let strcat a b =
  let len = string_length a + string_length b in
  string_init len (fun i ->
    if i < string_length a then
      string_get(a,i)
    else
      string_get(b,i - string_length a)
  );;

let stringrev(cs: string): string = 
    (* i is the index, starting from 0 *)
    let rec helprev revstr i =
      if i >= (string_length cs) then
        revstr
      else
        let new_revstr = strcat (str(string_get(cs,i))) revstr in
        helprev new_revstr (i+1)
    in
    helprev "" 0;;