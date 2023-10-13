(* ****** ****** *)
//
// How to test
// ./assign04_sol_dats
//
// How to compile:
// myatscc assign04_sol.dats
// or
// patscc -o assign04_sol_dats -DATS_MEMALLOC_LIBC assign04_sol.dats
//
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)

#include "./../assign04.dats"

(* ****** ****** *)

implement main0() = () where
{
val () =
println!
("Assign04: the first type-checker!") }

(* ****** ****** *)
(*
Please give your implementation below:
*)
(* ****** ****** *)

(* end of [assign04_sol.dats] *)
