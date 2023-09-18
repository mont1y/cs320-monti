#use "./../../../classlib/OCaml/MyOCaml.ml";; 
let string_avoid_132(cs:string): bool =
  let rec helper1 len1 i =
    if (i < len1) then
      let rec helper2 len2 j =
        if (j < len2) then
          if ord (string_get_at(cs)(i)) < ord (string_get_at(cs)(j)) then
            let rec helper3 len3 k =
              if (k < string_length cs) then
                if ord(string_get_at(cs)(k)) < ord(string_get_at(cs)(j)) && ord(string_get_at(cs)(k)) > ord(string_get_at(cs)(i)) then
                  false
                else
                  helper3 len3 (k+1)
              else
                helper2 len2 (j+1)
            in
              helper3 (string_length cs)(j+1)
          else
            helper2 len2 (j+1)
        else
          helper1 len1 (i+1)
      in
      helper2 (string_length cs) (i+1)
    else
      true
    
  in
  helper1 (string_length cs) 0;;

let () = Printf.printf "132: %b\n%!" (string_avoid_132("000"));;