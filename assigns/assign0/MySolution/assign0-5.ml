#use "./../assign0.ml";;

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