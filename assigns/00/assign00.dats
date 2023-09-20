(* ****** ****** *)
(*
** 
** Finding the number
** of bits in int-values
**
*)
(* ****** ****** *)
//
// HX: 10 points
// The function [gheep] is defined as follows:
//
fun ghaap(n: int): int =
(
  if
  (n >= 2)
  then n * ghaap(n-1) * ghaap(n-2)
  else (n+1)
  // end of [if]
)
//
// Please implement a tail-recursive function gheep
// such thats gheep(n) = ghaap(n) for all integers n
//
extern fun gheep(n: int): int
//
(* ****** ****** *)
//
datatype
intlist =
|
intlist_nil of ()
|
intlist_cons of (int, intlist)
//
#define nil intlist_nil
#define :: intlist_cons
#define cons intlist_cons
//
(* ****** ****** *)
//
// HX: 15 points
//
// intlist_append returns the concatenation
// of two given integer lists. For instance,
// given xs=(0,2,4) and ys = (1,3,5), then the
// returned list is (0, 2, 4, 1, 3, 5)
// Please give a tail-recursive implementation
// of intlist_append.
//
extern
fun
intlist_append : (intlist, intlist) -> intlist
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/assigns/00/assign00.dats] *)
