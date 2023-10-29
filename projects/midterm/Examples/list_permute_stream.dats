(* ****** ****** *)
#include "./mylib.dats"
(* ****** ****** *)
(*
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
*)
(* ****** ****** *)
//
fun
{x0:t@ype}
mylist_takeouts
(xs: mylist(x0)): mylist@(x0, mylist(x0)) =
(
case xs of
|
mylist_nil() =>
mylist_nil()
|
mylist_cons(x1, xs) =>
mylist_cons
(
@(x1, xs)
,
mylist_map<(x0,mylist(x0))><(x0,mylist(x0))>
( mylist_takeouts(xs)
, lam(xxs) => (xxs.0, mylist_cons(x1, xxs.1))))
)
//
(* ****** ****** *)
//
fun
{x0:t@ype}
mystream_concat_list
( xss
: mylist(mystream(x0))): mystream(x0) =
(
  auxmain(xss) ) where
{
fun
auxmain
( xss
: mylist(mystream(x0))): mystream(x0) = lam() =>
(
case+ xss of
|
mylist_nil() => myllist_nil()
|
mylist_cons(fxs, xss) => mystream_append<x0>(fxs, auxmain(xss))()) }
//
(* ****** ****** *)

fun
{x0:t@ype}
mylist_permute
(xs: mylist(x0)): mystream(mylist(x0)) = lam() =>
(
case+ xs of
|
mylist_nil() =>
myllist_sing(mylist_nil())
|
mylist_cons(_, _) =>
mystream_concat_list<mylist(x0)>
(mylist_map(mylist_takeouts(xs), lam(xxs) => mystream_map(mylist_permute(xxs.1), lam xs => mylist_cons(xxs.0, xs))))())

(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/list_permuate_stream.dats] *)
