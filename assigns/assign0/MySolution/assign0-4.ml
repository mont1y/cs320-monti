#use "./../assign0.ml";;

let str2int(cs: string): int =
  (* tr is the index count of the string, starting from the end, while i is the number I want to return, mul is the scaling index *)
  let rec getEach itr i mul =
    if itr <= 0 then
      i
    else
      if (string_get(cs,itr - 1)) = '-' then
        i
      else 
        let new_i = i + (((ord (string_get(cs,itr - 1))) - ord '0') * mul) in
        getEach (itr - 1) new_i (mul*10)
  in
  if string_get(cs,0) = '-' then
    -1*getEach (string_length cs) 0 1
  else
    getEach (string_length cs) 0 1
  ;;