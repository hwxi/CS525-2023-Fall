(* ****** ****** *)
#include
"share\
/atspre_staload.hats"
(* ****** ****** *)
#include "./mylib.dats"
(* ****** ****** *)
(*
Compile and test:
myatscc word_buddy_list.dats && ./word_buddy_list_dats
*)
(* ****** ****** *)
(*
//
Given a word w1 and another word w2, w1 and w2 are a
1-step doublet if w1 and w2 differ at exactly one position.
For instance, 'water' and 'later' are a 1-step doublet.
The doublet relation is the reflexive and transitive closure
of the 1-step doublet relation. In other words, w1 and w2 are
a doublet if w1 and w2 are the first and last of a sequence of
words where every two consecutive words form a 1-step doublet.
Here is a little website where you can use to check if two words
for a doublet or not:
http://ats-lang.github.io/EXAMPLE/BUCS320/Doublets/Doublets.html
//
The function [mylist_of_buddies] returns the list of all the
buddies of a given word (string):
//
fun
mylist_of_buddies(word: string): mylist(string)
//
Note that any string is considered a legal word, and no dictionary
is needed for implementing [mylist_of_buddies].
//
*)
(* ****** ****** *)
//
val AB = "abcdefghijklmnopqrstuvwxyz"
//
(* ****** ****** *)
//
fun
str_tabulate
( n0: int
, fopr: int -<cloref1> char): string =
string_make_mylist
(foreach_to_map_list(int_foreach)(n0, fopr))
//
(* ****** ****** *)
//
fun
str_fset_at
(cs: string, i0: int, c0: char) =
str_tabulate
(
str_length(cs),
lam i1 =>
if i1 != i0 then str_get_at(cs, i0) else c0)
//
(* ****** ****** *)
//
fun
mylist_of_buddies
(word: string): mylist(string) =
let
val n0 =
str_length(word)
in//let
mylist_make_fwork
(
lam work =>
int_foreach
(
n0,
lam i0 =>
let
val c0 = str_get_at(word, i0)
in//let
str_foreach
(
AB,
lam(c1) =>
if (c1 != c0) then work(str_fset_at(word, i0, c1)))
end//let//end-of-lam
) (* end-of-[int_foreach] *)
) (* end-of-[mylist_make_fwork] *)
end//let//end-of-[mylist_of_buddies]

(* ****** ****** *)
(* ****** ****** *)
implement main0() =
let
val
buddies =
mylist_of_buddies("love") in println!("buddies(love) = ", buddies)
end // let // end-of-[main0]
(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/word_buddy_list.dats] *)
