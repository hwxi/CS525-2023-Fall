/* ****** ****** */
// HX: 30 points
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
int_foldleft(n, 1, lam(r, i) => r * (i+1))

where 'int_foldleft' is defined as follows:

fun
int_foldleft
(n0, r0, fopr) =
(
  loop(0, r0)) where
{
fun
loop(i0) =
if (i0 >= n0) then r0 else loop(i0+1, fopr(r0, i0))
}

*/

/* ****** ****** */

/* end of [CS525-2023-Fall/assigns/assign05/Solution/facti_out.c] */
