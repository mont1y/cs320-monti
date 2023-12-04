#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | If of com list * com list | Bind
  | Lookup | Fun of com list | Call | Return

and coms = com list

and const =
  | Int of int
  | Bool of bool
  | Sym of string
  | Unit
  | Closure of string * (string * const) list * coms

let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')


let parse_symbol =
  (parse_nat >>= fun n -> fail) <|>
  (parse_int >>= fun _ -> fail) <|>
  (many1' (fun () -> satisfy is_alpha) >>= fun chars ->
    let sym_str = string_make_fwork (fun work -> list_foreach chars work) in
    pure (Sym sym_str)) 




let parse_unit =
  keyword "Unit" >> pure Unit

let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_symbol


let rec parse_com() = 
    (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
    (keyword "Pop" >> pure Pop) <|>
    (keyword "Swap" >> pure Swap) <|>
    (keyword "Trace" >> pure Trace) <|>
    (keyword "Add" >> pure Add) <|>
    (keyword "Sub" >> pure Sub) <|>
    (keyword "Mul" >> pure Mul) <|>
    (keyword "Div" >> pure Div) <|>
    (keyword "And" >> pure And) <|>
    (keyword "Or" >> pure Or) <|>
    (keyword "Not" >> pure Not) <|>
    (keyword "Lt" >> pure Lt) <|>
    (keyword "Gt" >> pure Gt) <|>
    parse_if() <|>
    (keyword "Bind" >> pure Bind) <|>
    (keyword "Lookup" >> pure Lookup) <|>
    parse_fun() <|>
    (keyword "Call" >> pure Call) <|>
    (keyword "Return" >> pure Return)

and parse_coms() = many' (fun x -> parse_com x << keyword ";")
and parse_if () = 
    let* _ = keyword "If" in
    let* if_branch = parse_coms() in
    let* _ = keyword "Else" in
    let* else_branch = parse_coms() in
    let* _ = keyword "End" in
    pure (If (if_branch, else_branch))
and parse_fun () =
    let* _ = keyword "Fun" in
    let* commands = parse_coms() in
    let* _ = keyword "End" in
    pure (Fun (commands))


(* ------------------------------------------------------------ *)

(* interpreter *)

type stack = const list
type trace = string list
type env = (string*const) list
type prog = coms


let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Sym s -> s
  | Unit -> "Unit"
  | Closure (s, e, p) -> string_append (string_append ("Fun<") (s)) (">")

let rec find x env =
  match env with
  | [] -> None
  | (sym, value) :: rest ->
    if sym = x then
      Some value
    else
      find x rest

let rec eval (s : stack) (t : trace) (e: env) (p : prog): trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t e p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 (* PopStack *) -> eval s0 t e p0
     | []      (* PopError *) -> eval [] ("Panic" :: t) e [])
  | Swap :: p0 -> 
    (match s with
     | c1 :: c2 :: s0 -> eval (c2 :: c1 :: s0) t e p0
     | [] -> eval [] ("Panic" :: t) e [] 
     | _ :: [] -> eval [] ("Panic" :: t) e [] )
  | Trace :: p0 ->
    (match s with
     | c :: s0 (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) e p0
     | []      (* TraceError *) -> eval [] ("Panic" :: t) e [])
  | Add :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t e p0
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) e []
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) e [])
  | Sub :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t e p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) e []
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) e [])
  | Mul :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t e p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) e []
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) e [])
  | Div :: p0 ->
    (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) e []
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t e p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) e []
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) e [])
  | And :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t e p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) e []
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) e [])
  | Or :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t e p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) e []
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) e [])
  | Not :: p0 ->
    (match s with
     | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t e p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) e []
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) e [])
  | Lt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t e p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) e []
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) e [])
  | Gt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t e p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) e []
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) e []
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) e [])
  | If(ifPart, elsePart) :: p0 -> 
    (match s with
     | Bool true :: s0 -> eval s0 t e (list_append ifPart p0) 
     | Bool false :: s0 -> eval s0 t e (list_append elsePart p0)
     | _ :: s0 -> eval [] ("Panic" :: t) e []
     | [] -> eval [] ("Panic" :: t) e [])
  | Bind :: p0 ->  
    (match s with
     | Sym st :: v :: s0 -> eval s0 t ((st, v) :: e) p0
     | _ :: s0 -> eval [] ("Panic" :: t) e []
     | [] -> eval [] ("Panic" :: t) e [])
  | Lookup :: p0 ->
    (match s with
     | Sym x :: s0 -> 
       (match find x e with
        | Some v -> eval (v :: s0) t e p0
        | None -> eval [] ("Panic" :: t) e [])
     | _ :: s0 -> eval [] ("Panic" :: t) e []
     | [] -> eval [] ("Panic" :: t) e [])
  | Fun (commands) :: p0 ->
    (match s with
     | Sym x :: s0 -> eval (Closure(x, e, commands) :: s0) t e p0
     | _ :: s0 -> eval [] ("Panic" :: t) e []
     | [] -> eval [] ("Panic" :: t) e [])
  | Call :: p0 ->
    (match s with
     | Closure (f, b, c) :: a :: s0 -> eval (a :: Closure("cc", e, p0) :: s0) t ((f, Closure (f, b, c)) :: b) (c)
     | _ :: s0 -> eval [] ("Panic" :: t) e []
     | [] -> eval [] ("Panic" :: t) e []
    )
  | Return :: p0 ->
    (match s with
     | Closure (f,b,c) :: a :: s0 -> eval (a :: s0) t (b) c
     | _ :: s0 -> eval [] ("Panic" :: t) e []
     | [] -> eval [] ("Panic" :: t) e []
     )

(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms()) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None