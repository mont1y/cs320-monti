let rec map f = function
  | [] -> []
  | head :: tail -> f head :: map f tail
;;

let rec list_hd lst =
  match lst with
  | [] -> raise (Failure "empty list")
  | x :: _ -> x

let rec map_list_hd matrix =
  match matrix with
  | [] -> []
  | row :: rest -> list_hd row :: map_list_hd rest

let rec list_tl lst =
  match lst with
  | [] -> raise (Failure "empty list")
  | _ :: xs -> xs

let rec map_list_tl matrix =
  match matrix with
  | [] -> []
  | row :: rest -> list_tl row :: map_list_tl rest

let rec matrix_transpose (xss : 'a list list) : 'a list list =
  let rec transpose_row matrix =
    match matrix with
    | [] -> []
    | [] :: _ -> []
    | _ -> (map_list_hd matrix) :: transpose_row (map_list_tl matrix)
  in
    transpose_row xss