(* ****** ****** *)
(*
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
*)
(* ****** ****** *)

fun
stream_nil
((*void*)) =
fn() => llist_nil

(* ****** ****** *)

fun
stream_sing(x0) =
llist_cons(x0, stream_nil())

(* ****** ****** *)

fun
list_map(xs, fopr) =
list_foldright(xs, list_nil, fn(x, ys) => fopr(x) :: ys)

(* ****** ****** *)

fun
list_takeouts
(xs: 'a list): ('a * 'a list) list =
case xs of
|
list_nil => list_nil
|
list_cons(x1, xs) =>
(x1, xs) :: list_map(list_takeouts(xs), fn(x2, xs) => (x2, x1 :: xs))

(* ****** ****** *)

fun
list_permute
(xs) = fn() =>
case xs of
| list_nil =>
llist_sing([])
| list_cons(_, _) =>
stream_concat_list
(list_map(list_takeouts(xs), fn(x1, xs) => stream_map(list_permute(xs), fn xs => x1 :: xs)))()

(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/list_permuate_stream.sml] *)
