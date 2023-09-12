(* ****** ****** *)
(*
Assign0: Warmup!
*)
(* ****** ****** *)

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)


let rec fact(x: int): int =
if x > 0 then x * fact(x-1) else 1;;

(* String concat helped function *)

let strcat a b =
  let len = String.length a + String.length b in
  String.init len (fun i ->
    if i < String.length a then
      a.[i]
    else
      b.[i - String.length a]
)

(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)
let rec
myans(x:int): int =
if fact(x) = 0 then x else myans(x+1)
;;

let () = Printf.printf "overflow: %i\n%!" (myans(0));;


(* let () =
if (myans <> 64) then failwith "Assign0-1"
(* ****** ****** *)
let () =
print_string("Assign0-1-test passed!\n")
(* ****** ****** *)
;; *)


(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)


(* else if x mod 2 = 0 then false
else if x mod 3 = 0 then false
else if x mod 4 = 0 then false
...
else if n*>x = 0 then true *)
let 
isPrime(x:int): bool =
  let rec loopy(n:int): bool = 
    if n * n > x then true
    else if x mod n = 0 then false
    else loopy(n+1)
  in
  if x < 2 then false 
  else if x == 2 then true
  else if loopy(2) = true then true
  else false
  ;;

let () = Printf.printf "isPrime: %b\n%!" (isPrime(63));;

(* ****** ****** *)(* ****** ****** *)

let () =
assert(isPrime(5) == true)
let () =
assert(isPrime(7) == true)
let () =
assert(isPrime(57) == false)
let () =
assert(isPrime(101) == true)
let () =
assert(isPrime(1001) == false)
let () =
assert(isPrime(10001) == false)

(* ****** ****** *)
let () = print_string("Assign0-2-test passed!\n")
(* ****** ****** *)
;;
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-2-test.sml] *)


(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

(* let my_char = chr ((i0 mod 10) + 48) in  *)

let int2str(i0: int): string = 
  let rec getEach n conc=
    if (n < 10) then
      (* Add Digit to String *)
      strcat (str(chr (n + ord '0'))) conc
    else
      let num = n mod 10 in
      let new_conc = strcat (str (chr (num + ord '0'))) conc in
      (* Add this char to String *)
      getEach (n / 10) new_conc
  in
  if i0 = 0 then "0" 
  else if i0 < 0 then strcat "-"  (getEach (abs i0) "")
  else getEach i0 "";;

let () = Printf.printf "WTF: %s\n%!" (int2str(0));;

(* My Problemo: How the f wdo I convert characters to string and concatenate them together*)

(* let s1 = "1";;


let str(c0) = String.make 1 c0;;
let str s1 = String.init (String.length s1) (fun i -> s1.[i]);; *)
(* let s1 = "1";;
let s2 = "2";;
let c = 3;;
let wtf(x:int): char = Char.chr (c + ord '0');;
let s3 = string_init (1) (wtf);;
let () = Printf.printf "WTF: %s\n%!" (s1^s2);; *)
(* ****** ****** *)
(* ****** ****** *)

let () = assert(int2str(0) = "0")
let () = assert(int2str(10) = "10")
let () = assert(int2str(100) = "100")
let () = assert(int2str(1000) = "1000")
let () = assert(int2str(12345) = "12345")

(* ****** ****** *)
let () =
print_string("Assign0-3-test passed!\n")
(* ****** ****** *)
;;
(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

let str2int(cs: string): int =
  (* tr is the index count of the string, starting from the end, while i is the number I want to return, mul is the scaling index *)
  let rec getEach itr i mul =
    if itr <= 0 then
      i
    else
      if (string_get(cs, itr - 1)) = '-' then
        i
      else 
        let new_i = i + (((ord (string_get(cs, itr - 1))) - ord '0') * mul) in
        getEach (itr - 1) new_i (mul*10)
  in
  if string_get(cs, 0) = '-' then
    -1*getEach (string_length cs) 0 1
  else
    getEach (string_length cs) 0 1

let () = Printf.printf "str2int: %i\n%!" (str2int("-12"));;
let () = Printf.printf "str2int: %i\n%!" (str2int(int2str(-2123)));;

(* ****** ****** *)

let () = assert(str2int("0") = 0)
let () = assert(str2int("10") = 10)
let () = assert(str2int("100") = 100)
let () = assert(str2int("1234") = 1234)

(* ****** ****** *)
let () =
print_string("Assign0-4-test passed!\n")
(* ****** ****** *)
;;



(* ****** ****** *)

(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)

let stringrev(cs: string): string = 
    (* i is the index, starting from 0 *)
    let rec helprev revstr i =
      if i >= (string_length cs) then
        revstr
      else
        let new_revstr = strcat (str(string_get(cs, i))) revstr in
        helprev new_revstr (i+1)
    in
    helprev "" 0;;

let () = Printf.printf "strrev: %s\n%!" (stringrev("safkbhdssfh"));;

(* ****** ****** *)

let () = assert(stringrev("") = "")
let () = assert(stringrev("1") = "1")
let () = assert(stringrev("12") = "21")
let () = assert(stringrev("123") = "321")
let () = assert(stringrev("1234") = "4321")
let () = assert(stringrev("12345") = "54321")

(* ****** ****** *)
let () = print_string("Assign0-5-test passed!\n")
(* ****** ****** *)
;;

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0.ml] *)