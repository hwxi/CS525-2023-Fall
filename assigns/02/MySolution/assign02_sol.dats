(* ****** ****** *)
//
// How to test
// ./assign02_sol_dats
//
// How to compile:
// myatscc assign02_sol.dats
// or
// patscc -o assign02_sol_dats -DATS_MEMALLOC_LIBC assign02_sol.dats
//
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)

#include "./../assign02.dats"

(* ****** ****** *)

implement main0() = ((*dummy*))

(* ****** ****** *)

(* end of [assign02_sol.dats] *)
