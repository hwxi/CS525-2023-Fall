(* ****** ****** *)
(*
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
*)
(* ****** ****** *)
#include "./mylib.dats"
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
