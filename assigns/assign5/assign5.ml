#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)
let rec parse_expr cs =
  match trim cs with
  | '(' :: 'a' :: 'd' :: 'd' :: rest ->
      let exprs, remaining = parse_exprs rest in
      Some (Add exprs), remaining
  | '(' :: 'm' :: 'u' :: 'l' :: rest ->
      let exprs, remaining = parse_exprs rest in
      Some (Mul exprs), remaining
  | '(' :: rest ->
      let expr, remaining = parse_expr rest in
      (match remaining with
      | ')' :: tail -> Some expr, tail
      | _ -> None)
  | _ -> parse_num cs

and parse_exprs cs =
  let expr, remaining = parse_expr cs in
  match remaining with
  | ')' :: tail -> [expr], tail
  | _ ->
      let exprs, remaining' = parse_exprs remaining in
      expr :: exprs, remaining'

and parse_num cs =
  let rec parse_digits acc cs =
    match cs with
    | c :: rest when Char.is_digit c ->
        parse_digits (c :: acc) rest
    | _ -> (
        match String.of_char_list (List.rev acc) with
        | "" -> None
        | num -> Some (Int (int_of_string num)), cs
      )
  in
  parse_digits [] cs

let parse (s : string) : expr option =
  match parse_expr (string_listize s) with
  | Some expr, [] -> Some expr
  | _ -> None


  let () =
  let test_case s =
    match parse s with
    | Some expr -> Printf.printf "Parsed: %s\n" (MyOCaml.to_string (expr :> MyOCaml.value))
    | None -> Printf.printf "Invalid input: %s\n" s
  in

  (* Test cases *)
  let valid_inputs = ["(add 1 2 3)"; "(mul (add 1 2) 3 (mul 1))"] in
  let invalid_inputs = ["()"; "(add)"; "(add 1 2))"; "((mul 1 2)"] in

  Printf.printf "Valid inputs:\n";
  List.iter test_case valid_inputs;

  Printf.printf "\nInvalid inputs:\n";
  List.iter test_case invalid_inputs