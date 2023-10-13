(* ****** ****** *)
(*
Due: Wednesday, the 18th of October
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
HX-2023-10-12: 50 points (Due: 2023-10-18)
//
Please study the code in lecture-10-11 and then
copy/paste/modify it. Afterwards, please construct
a type-checker of the following type:
//
extern fun assign03_tpcheck(t0: term): type
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
//
typedef tbas = string
//
(* ****** ****** *)
//
datatype type =
//
|
TPbas of tbas // base types:
// int, bool, float, string, etc
|
TPfun of (type, type) // T1 -> T2
|
TPtup of (type, type) // T1 * T2
//
(*
| TPref of type
| TParray of type
| TPlist of type // for lists
| TPllist of type // for lazy lists
*)
//
where typelst = mylist(type)
//
(* ****** ****** *)
val TPint = TPbas("int")
val TPbtf = TPbas("bool")
val TPstr = TPbas("string")
(* ****** ****** *)
extern
fun
print_type(type): void
extern
fun
fprint_type
(out:FILEref, tp:type): void
(* ****** ****** *)
implement
print_type(tp) =
fprint_type(stdout_ref, tp)
(* ****** ****** *)
implement
fprint_val<type> = fprint_type
(* ****** ****** *)
overload print with print_type
overload fprint with fprint_type
(* ****** ****** *)
implement
fprint_type
(out, tp) =
(
case+ tp of
|
TPbas(nm) =>
fprint!(out, "TPbas(", nm, ")")
|
TPfun(tp1, tp2) =>
fprint!(out, "TPfun(", tp1, ";", tp2, ")")
|
TPtup(tp1, tp2) =>
fprint!(out, "TPtup(", tp1, ";", tp2, ")")
)
(* ****** ****** *)
//
extern
fun
type_equal
(t1: type, t2: type): bool
overload = with type_equal
//
(* ****** ****** *)
//
implement
type_equal
( t1, t2 ) =
(
case+ t1 of
|
TPbas(nm1) =>
(case+ t2 of
|
TPbas(nm2) => (nm1 = nm2) | _ => false)
|
TPfun(t11, t12) =>
(case+ t2 of
|
TPfun(t21, t22) =>
( t11 = t21 && t12 = t22 ) | _ => false)
|
TPtup(t11, t12) =>
(case+ t2 of
|
TPtup(t21, t22) =>
( t11 = t21 && t12 = t22 ) | _ => false)
)
//
(* ****** ****** *)
typedef tvar = string
typedef topr = string
(* ****** ****** *)
//
datatype term =
//
| TMint of int
| TMbtf of bool
| TMstr of string
//
| TMvar of tvar
| TMlam of (tvar, term)
| TMapp of (term, term)
//
| TMopr of (topr, termlst)
//
| TMif0 of (term, term, term)
//
| TMlet of
( tvar(*x*)
, term(*t1*), term(*t2*))
//
| TMfix of
  (tvar(*f*), tvar(*x*), term)
//
| TMlam2 of
  (tvar, type, term)
| TMfix2 of
  (tvar(*f*), tvar(*x*), type, term)
//
where termlst = mylist(term)
//
(* ****** ****** *)

typedef tpctx = mylist(@(tvar, type))

(* ****** ****** *)

extern
fun
print_term(t0:term): void
extern
fun
fprint_term
(out:FILEref, t0:term): void
(* ****** ****** *)
implement
print_term(t0) =
fprint_term(stdout_ref, t0)
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
TMstr(s0) =>
fprint!(out, "TMstr(", s0, ")")
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
TMopr(nm, ts) =>
fprint!(out, "TMopr(", nm, ";", ts, ")")
|
TMif0(t1, t2, t3) =>
fprint!
(out, "TMif0(", t1, ";", t2, ";", t3, ")")
//
|
TMlet(x, t1, t2) =>
fprint!
(out, "TMlet(", x, ";", t1, ";", t2, ")")
|
TMfix(f, x, tt) =>
fprint!(out, "TMfix(", f, ";", x, ";", tt, ")")
//
|
TMlam2(x, Tx, tt) =>
fprint!(out, "TMlam2(", x, ";", Tx, ";", tt, ")")
|
TMfix2(f, x, Tf, tt) =>
fprint!(out, "TMfix2(", f, ";", x, ";", Tf, ";", tt, ")")
//
)
//
(* ****** ****** *)

extern
fun
term_type0(term): type
extern
fun
term_type1(term, tpctx): type
extern
fun
term_type1_ck
(t0: term, T0: type, ctx: tpctx): void
extern
fun
termlst_type1
(ts:termlst, e0:tpctx): typelst

(* ****** ****** *)

extern
fun
tpctx_lookup(tpctx, tvar): type

(* ****** ****** *)

(* end of [CS525-2023-Fall/assigns/assign04.dats] *)
