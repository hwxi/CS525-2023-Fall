/* ****** ****** */
// HX: 20 points
/* ****** ****** */

#include "runtime2.h"

/* ****** ****** */

extern
void*
mymalloc(size_t n) {
  void* p0;
  p0 = malloc(n);
  if (p0 != 0) return p0;
  fprintf(stderr, "myalloc failed!!!\n");
  exit(1);
}

/* ****** ****** */

/*
HX-2023-11-05:
Please translate the following function
into C code of the style given in fact_out.c

fun
fact2(n:int): int =
let
//
(*
How do you handle an inner function like
loop in your translation?
*)
//
fun
loop(i: int, r: int): int =
if i < n
then loop(i+1, (i+1) * r) else r
//
in
  loop(0, 1)
end
*/

/* ****** ****** */

/* end of [CS525-2023-Fall/assigns/assign05/Solution/fact2_out.c] */
