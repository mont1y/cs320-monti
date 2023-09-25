#use "./../../../classlib/OCaml/MyOCaml.ml";; 

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


let rec mylist_reverse (xs: 'a mylist): 'a mylist =
  match xs with
  | MyNil -> MyNil
  | MyCons (x, xs) -> MySnoc (mylist_reverse xs, x)
  | MySnoc (xs, x) -> MyCons (x, mylist_reverse xs)
  | MyReverse xs -> xs
  | MyAppend2 (xs1, xs2) -> MyAppend2 (mylist_reverse xs2, mylist_reverse xs1)

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