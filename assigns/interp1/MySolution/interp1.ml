#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)






type const =
  | Int of int  
  | Bool of bool
  | Unit
type com =
  | Push of const
  | Pop
  | Trace
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Not
  | Lt
  | Gt

let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let string_of_list (cs : char list) : string =
  string_make_fwork(fun work -> list_foreach cs work)

let option1 (p : 'a parser) : 'a option parser =
  fun xs ->
    match p xs with
    | Some (a, xs) -> Some (Some a, xs)
    | None -> Some (None, xs)

let parse_const =
  let parse_int =
    let* sign = option1 (char '-') in
    let* n = natural in
    let value = match sign with
      | Some _ -> -n
      | None -> n
  in
  pure (Int value) << whitespaces
  in
  let parse_bool =
    (keyword "True" >> pure (Bool true)) <|> (keyword "False" >> pure (Bool false))
  in
  let parse_unit = 
    (keyword "Unit" >> pure(Unit))
  in
  parse_int <|> parse_bool <|> parse_unit

let parse_com =
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c))
  <|>
  (keyword "Pop" >> pure(Pop))
  <|>
  (keyword "Trace" >> pure(Trace))
  <|>
  (keyword "Add" >> pure(Add))
  <|>
  (keyword "Sub" >> pure(Sub))
  <|>
  (keyword "Mul" >> pure(Mul))
  <|>
  (keyword "Div" >> pure(Div))
  <|>
  (keyword "And" >> pure(And))
  <|>
  (keyword "Or" >> pure(Or))
  <|>
  (keyword "Not" >> pure(Not))
  <|>
  (keyword "Lt" >> pure(Lt))
  <|>
  (keyword "Gt" >> pure(Gt))
  
let string_of_const (c : const) : string =
  match c with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "Unit"
  
let rec string_of_com (e : com list) : string =
  match e with
  | Push c :: rest -> "Push " ^ string_of_const c ^ ";" ^ string_of_com rest
  | Pop :: rest -> "Pop;" ^ string_of_com rest
  | Trace :: rest -> "Trace;" ^ string_of_com rest
  | Add :: rest -> "Add;" ^ string_of_com rest
  | Sub :: rest -> "Sub;" ^ string_of_com rest
  | Mul :: rest -> "Mul;" ^ string_of_com rest
  | Div :: rest -> "Div;" ^ string_of_com rest
  | And :: rest -> "And;" ^ string_of_com rest
  | Or :: rest -> "Or;" ^ string_of_com rest
  | Not :: rest -> "Not;" ^ string_of_com rest
  | Lt :: rest -> "Lt;" ^ string_of_com rest
  | Gt :: rest -> "Gt;" ^ string_of_com rest
  | [] -> ""


(* let parsed_commands = [Push (Int 1); Pop] *)

let parse_commands (s: string) : com list option =
  match string_parse (many (parse_com << keyword ";")) s with
  | Some (commands, _) -> Some commands
  | None -> None
(* 
let () =
  match parse_commands "Push Unit; Pop; Push -21; Lt; Sub; Div; Push Unit;" with
  | Some parsed_commands ->
    let output = string_of_com parsed_commands in
    print_endline output
  | None ->
    print_endline "Failed to parse commands." *)

let rec intrep1 (stack: const list) (trace: string list) (program: com list) : string list =
    match program with
    | Push c :: program -> intrep1 (c :: stack) (trace) (program)
    | Pop :: program -> (
        match stack with
        | _ :: rest -> intrep1 (rest) (trace) (program)
        | [] -> "Panic" :: trace
      )
    | Trace :: program -> (
        match stack with
        | c :: rest -> intrep1 (Unit :: rest) (string_of_const c :: trace) (program)
        | [] -> "Panic" :: trace
      )
    | Add :: program -> (
        match stack with
        | Int a :: Int b :: rest -> intrep1 (Int (a+b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Sub :: program -> (
        match stack with
        | Int a :: Int b :: rest -> intrep1 (Int (a-b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Mul :: program -> (
        match stack with
        | Int a :: Int b :: rest -> intrep1 (Int (a*b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Div :: program -> (
        match stack with
        | Int a :: Int b :: rest -> (
          if b = 0 then "Panic" :: trace
          else intrep1 (Int (a/b) :: rest) (trace) (program)
        )
        | _ -> "Panic" :: trace
      )
    | And :: program -> (
        match stack with
        | Bool a :: Bool b :: rest -> intrep1 (Bool (a && b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Or :: program -> (
        match stack with
        | Bool a :: Bool b :: rest -> intrep1 (Bool (a || b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Not :: program -> (
        match stack with
        | Bool a :: rest -> intrep1 (Bool (not a) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Lt :: program -> (
        match stack with
        | Int a :: Int b :: rest -> intrep1 (Bool (a < b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | Gt :: program -> (
        match stack with
        | Int a :: Int b :: rest -> intrep1 (Bool (a > b) :: rest) (trace) (program)
        | _ -> "Panic" :: trace
      )
    | [] -> trace
;;

let interp (s : string) : string list option = 
  match parse_commands (string_of_list(trim (string_listize s))) with
  | Some commands -> Some (intrep1 [] [] commands)
  | None -> None

let () =
  match interp "Push 1; Add; Trace;" with
  | Some interp ->
    Printf.printf "Interpreted result: %s\n" (String.concat " " interp)
  | None ->
    print_endline "Failed to parse commands."
