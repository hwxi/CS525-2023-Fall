(* ****** ****** *)
(*
Due: Wednesday, the 4th of October
*)
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload "./../../mylib/mylib.dats"
(* ****** ****** *)
(*
implement main() = 0 // HX: this is a dummy
*)
(* ****** ****** *)
(*
//
HX-2023-10-04: 50 points (Due: 2023-10-11)
//
Please study the code in lecture-10-04 and then
copy/paste/modify it. Afterwards, please construct
a compiler that compiles the LAMBDA1 (which extends
the pure lambda-calculus) into the pure core that
contains only three constructs: TMvar, TMlam, and
TMapp. The name of the main compiling function is
given below:
//
extern
fun assign03_compile(t0: term): term
//
Please note that new constructs TMtup, TMfst, and
TMsnd are added for representing tuples. You need
to first implement these constructs in term_eval1:
//
|
TMtup(t1, t2) =>
VALtup(v1, v2) where
{
  val v1 = term_eval1(t1, e0)
  val v2 = term_eval1(t2, e0)
}
|
TMfst(tt) => // fst projection
(
case- vv of
|VALtup(v1, v2) => v1
) where
{
  val vv = term_eval1(tt, e0) }
|
TMsnd(tt) => // snd projection
(
case- vv of
|VALtup(v1, v2) => v2
) where
{
  val vv = term_eval1(tt, e0) }
//
Please note that you need to compile TMint and TMbtf
into Church numerals and Church booleans. In addition,
the primitive operators need to be implemented accordingly
Also, the handling of TMif0 can be a little bit tricky
because LAMBDA1 is a call-by-value language.
//
*)
(* ****** ****** *)
#include
"share\
/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../mylib/mylib.dats"
(* ****** ****** *)
typedef tvar = string
typedef topr = string
(* ****** ****** *)
datatype term =
//
| TMint of int
| TMbtf of bool
//
| TMvar of tvar
| TMlam of (tvar, term)
| TMapp of (term, term)
//
| TMfst of term
| TMsnd of term
| TMtup of (term, term)
//
| TMopr of (topr, termlst)
//
| TMif0 of (term, term, term)
//
where termlst = mylist(term)
//
(* ****** ****** *)

datatype
value =
//
| VALint of int
| VALbtf of bool
//
| VALtup of (value, value)
//
| VALlam of (term(*lam*), envir)

where envir = mylist(@(tvar, value))

(* ****** ****** *)

typedef
valuelst = mylist(value)

(* ****** ****** *)
extern
fun
print_term(t0:term): void
extern
fun
fprint_term
(out:FILEref, t0:term): void
(* ****** ****** *)
extern
fun
print_value(v0:value): void
extern
fun
fprint_value
(out:FILEref, v0:value): void
(* ****** ****** *)
//
implement
print_term(t0) =
fprint_term(stdout_ref, t0)
//
(* ****** ****** *)
implement
fprint_val<term> = fprint_term
(* ****** ****** *)
overload print with print_term
overload fprint with fprint_term
(* ****** ****** *)
//
implement
fprint_term
(out, t0) =
(
case+ t0 of
|
TMint(i0) =>
fprint!(out, "TMint(", i0, ")")
|
TMbtf(b0) =>
fprint!(out, "TMbtf(", b0, ")")
|
TMvar(v0) =>
fprint!(out, "TMvar(", v0, ")")
|
TMlam(v0, t1) =>
fprint!(out, "TMlam(", v0, ";", t1, ")")
|
TMapp(t1, t2) =>
fprint!(out, "TMapp(", t1, ";", t2, ")")
//
|
TMfst(tt) =>
fprint!(out, "TMfst(", tt, ")")
|
TMsnd(tt) =>
fprint!(out, "TMsnd(", tt, ")")
|
TMtup(t1, t2) =>
fprint!(out, "TMtup(", t1, ";", t2, ")")
//
|
TMopr(nm, ts) =>
fprint!(out, "TMopr(", nm, ";", ts, ")")
|
TMif0(t1, t2, t3) =>
fprint!(out, "TMif0(", t1, ";", t2, ";", t3, ")")
)
//
(* ****** ****** *)
implement
print_value(t0) =
fprint_value(stdout_ref, t0)
(* ****** ****** *)
implement
fprint_val<value> = fprint_value
(* ****** ****** *)
overload print with print_value
overload fprint with fprint_value
(* ****** ****** *)
//
implement
fprint_value
(out, v0) =
(
case+ v0 of
|
VALint(i0) =>
fprint!(out, "VALint(", i0, ")")
|
VALbtf(b0) =>
fprint!(out, "VALbtf(", b0, ")")
|
VALtup(v1, v2) =>
fprint!(out, "VALtup(", v1, ";", v2, ")")
|
VALlam(t1, env) =>>
fprint!(out, "VALbtf(", t1, ";", "...", ")")
)
//
(* ****** ****** *)

extern
fun
assign03_compile(t0: term): term

(* ****** ****** *)

(*
HX-2023-10-04:
Some testing code will be given later.
*)

(* ****** ****** *)

(* end of [CS525-2023-Fall/assigns/assign03.dats] *)
