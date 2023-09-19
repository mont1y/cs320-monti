#use "./../../../classlib/OCaml/MyOCaml.ml";; 
let string_longest_ascend(xs: string): string =
  (* curLong is the current longest consecutive string
     count is the count of the length of the current consecutive string 
     i is the index on the string*)
  let rec helper finLong newLong i =
    if (i < string_length xs) then
      let rec helper2 (curLong: string) (j:int): string =
        if (j < string_length xs) then
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
      finLong
  in
  helper "" "" 0
;;