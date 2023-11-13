#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<sexpr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<sexpr>  ::= <num> 
          | (SAdd <sexprs> )
          | (SMul <sexprs> )
<sexprs> ::= <sexpr> | <sexpr><sexprs>

123
(SAdd 123 123)
(SAdd (SMul 1 2) 123)

((SAdd 123 123)
(123 123)

*)

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* turn a string SInto a list of chars *)
(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let string_of_list (cs : char list) : string =
  string_make_fwork(fun work -> list_foreach cs work)

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

let rec parse_expr () : sexpr parser =
  parse_int () <|> parse_add () <|> parse_mul ()

and parse_int () : sexpr parser =
  let* n = natural in
  pure (SInt n) << whitespaces

and parse_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SAdd es)

and parse_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SMul es)

let sexpr_parse (s : string) : sexpr option =
  match string_parse (parse_expr ()) s with
  | Some (e, []) -> Some e
  | _ -> None

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit
let foldleft_to_iforeach(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  fun(xs)(work) -> 
      let _ = foldleft(xs)(0)(fun (r)(x) -> (work(r)(x);r+1))
  in
  ();;
let
list_iforeach =
fun xs -> foldleft_to_iforeach(list_foldleft)(xs)
;;

let
string_sepjoin_list
(sep: string)(xs: string list): string =
 string_make_fwork(
  fun work ->
    list_iforeach xs (fun i x -> (if i > 0 then (string_foreach sep work)); string_foreach x work)
  )
  ;;

let sexpr_to_string (e : sexpr) : string =
  let rec sexpr_to_string_rapper e = 
    match e with
    | SInt n -> str (chr (ord('0') + n))
    (* Inside: add everthing in between with a space seperating them all using the foldleft function *)
    | SAdd es -> string_append("(add ")(string_append(string_sepjoin_list(" ")(list_reverse(list_foldleft es [] (fun acc x -> sexpr_to_string_rapper x :: acc))))(")"))  
    | SMul es -> string_append("(mul ")(string_append(string_sepjoin_list(" ")(list_reverse(list_foldleft es [] (fun acc x -> sexpr_to_string_rapper x :: acc))))(")"))
  in
  sexpr_to_string_rapper e


let test1 = sexpr_to_string(SAdd [SInt 1; SInt 2; SInt 3])
let () = assert(test1 = "(add 1 2 3)")
let () = assert (sexpr_parse(test1) = Some (SAdd [SInt 1; SInt 2; SInt 3]))


let test2 = sexpr_to_string(SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
let () = assert(test2 = "(mul (add 1 2) 3 (mul 1))")
let () = assert(sexpr_parse(test2) = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]]))

let test3 = sexpr_to_string(SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1; SInt 2; SInt 3]])
let () = assert(test3 = "(mul (add 1 2) 3 (mul 1 2 3))")
let () = assert(sexpr_parse(test3) = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1; SInt 2; SInt 3]]))

(* ****** ****** *)

let test4 = sexpr_parse("(add 1 2))")
let () = assert(test4 = None)

let test5 = sexpr_parse("(add 1 2 3")
let () = assert(test5 = None)

let test6 = sexpr_parse("(add 1 ()()))))2 3))")
let () = assert(test6 = None)

(* ****** ****** *)

let () = print_string("Assign6-1-test passed!\n")
;;
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign6-1-test.sml] *)


(* ****** ****** *)

(* end of [CS320-2023-Fall-assign6-1-test.sml] *)