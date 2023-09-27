#use "./../../../classlib/OCaml/MyOCaml.ml";; 
(* ****** ****** *)
(*
//
Assign2: datatypes and more loops
//
DUE: the 27th of September, 2023
//
Total: 80 point
(OCaml: 40 points)(Python: 40 points)
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

(* ****** ****** *)
exception MySubscript;;
(* ****** ****** *)

(*
HX-2023-09-21:
Please read the following code to understand
the meaning of mylist.
*)

(* ****** ****** *)

let rec
mylist_foreach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (work(x1); mylist_foreach(xs)(work))
| MySnoc(xs, x1) ->
  (mylist_foreach(xs)(work); work(x1))
| MyReverse(xs) -> mylist_rforeach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

and
mylist_rforeach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (mylist_rforeach(xs)(work); work(x1))
| MySnoc(xs, x1) ->
  (work(x1); mylist_rforeach(xs)(work))
| MyReverse(xs) -> mylist_foreach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))
;;
(* ****** ****** *)




let rec mylist_reverse (xs: 'a mylist): 'a mylist =
  match xs with
  | MyNil -> MyNil
  | MyCons (x, xs) -> MySnoc (mylist_reverse xs, x)
  | MySnoc (xs, x) -> MyCons (x, mylist_reverse xs)
  | MyReverse xs -> xs
  | MyAppend2 (xs1, xs2) -> MyAppend2 (mylist_reverse xs2, mylist_reverse xs1)


let rec print_mylist : 'a mylist -> unit = function
  | MyNil -> ()
  | MyCons (head, tail) ->
      print_int head;
      print_string ", ";
      print_mylist tail
  | MySnoc (init, last) ->
      print_mylist init;
      print_int last;
      print_string ", "
  | MyReverse lst ->
      print_string "Reversed: ";
      print_mylist (mylist_reverse(lst))
  | MyAppend2 (list1, list2) ->
      print_mylist list1;
      print_mylist list2

(*
//
Assign2-1: 10 points
//
Please implement mylist_length based
on pattern matching that computes the
length of a given mylist.
//
let rec
mylist_length(xs: 'a mylist): int = ...
//
*)

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(x1, xs) -> 1 + mylist_length xs
  | MySnoc(xs, x1) -> 1 + mylist_length xs
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2

let mylist123 = MyCons (0, MyCons (1, MyCons (2, MyCons (3, MyNil))))

let () = Printf.printf "list_length: %i\n%!" (mylist_length(mylist123));;
let () = print_mylist mylist123;;
let () = Printf.printf "\n%!";;

let xs0 = MyNil
let xs1 = MyCons(1, xs0)
let xs2 = MySnoc(xs0, 1)
let xs3 = MyAppend2(xs1, xs2)
let xs4 = MyReverse(xs3)
let xs5 = MyAppend2(xs4, xs4)
let xs6 = MyAppend2(xs5, xs5)
let xs7 = MyAppend2(xs6, xs6)
;;
(* ****** ****** *)
let () = assert(mylist_length(xs7) = 16)
;;
(* ****** ****** *)
let () = print_string("Assign2-1-test passed!\n")


(* ****** ****** *)
let
mylist_subscript_exn
( (*void*) ): 'a = raise MySubscript;;
(* ****** ****** *)

(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)

let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a =
  match xs with
  | MyNil -> mylist_subscript_exn()
  | MyCons(x1, xs) -> 
    if (i0 == 0) then 
      x1 
    else
      mylist_get_at(xs)(i0-1)
  | MySnoc(xs, x1) ->
    mylist_get_at(MyAppend2(xs, MyCons(x1, MyNil)))(i0)
  | MyReverse(xs) ->
    (* How do I dissect a revserse? my_lsit_get_at(reverse of the list)
    Reverse of the list =  *)
    mylist_get_at(mylist_reverse(xs))(i0)
  | MyAppend2(xs1, xs2) ->
    if (i0 > mylist_length xs1 - 1) then
      mylist_get_at(xs2)(i0 - mylist_length xs1)
    else
      mylist_get_at(xs1)(i0)
;;

let mylisttest = MyReverse(MyCons (1, MyCons (1, MySnoc (MyCons (3, MyNil), 6))));;
let () = print_mylist mylisttest;;
let () = Printf.printf "\n%!";;
let () = Printf.printf "element at 3: %i\n%!" (mylist_get_at mylisttest 3);;
(* ****** ****** *)


(* ****** ****** *)



type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit


(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* ****** ****** *)
let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;;
(* ****** ****** *)

let foldleft_to_iforeach(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  fun(xs)(work) -> 
      let _ = foldleft(xs)(0)(fun (r)(x) -> (work(r)(x);r+1))
  in
  ();;
  
(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list(".")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)

(* ****** ****** *)

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
  (* ****** ****** *)
  
  (* end of [CS320-2023-Fall-assign2-4-test.sml] *)
(* end of [CS320-2023-Fall-assigns-assign2.ml] *)
(* ["str1", "str2"]*)