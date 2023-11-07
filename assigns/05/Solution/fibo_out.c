/* ****** ****** */
// HX: 10 points
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

fun fibo(n: int): int =
if n >= 2 then fibo(n-2)+fibo(n-1) else n
*/

/* ****** ****** */

/* end of [CS525-2023-Fall/assigns/assign05/Solution/fibo_out.c] */
