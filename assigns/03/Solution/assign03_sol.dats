(* ****** ****** *)
//
// How to test
// ./assign03_sol_dats
//
// How to compile:
// myatscc assign03_sol.dats
// or
// patscc -o assign03_sol_dats -DATS_MEMALLOC_LIBC assign03_sol.dats
//
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)

#include "./../assign03.dats"

(* ****** ****** *)

implement main0() = () where
{
val () =
println!
("Assign03: the first compiler!") }

(* ****** ****** *)
(*
Please give your implementation below:
*)
(* ****** ****** *)

(* end of [assign03_sol.dats] *)
