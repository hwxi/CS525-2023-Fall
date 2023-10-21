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
TPxyz of txyz
//
|
TPfun of (type, type) // T1 -> T2
|
TPtup of (type, type) // (T1 * T2)
//
(*
| TPref of type
| TParray of type
| TPlist of type // for lists
| TPllist of type // for lazy lists
*)
//
where
typelst = mylist(type)
and
typeopt = myoptn(type)
//
and txyz = ref(typeopt)
//
(* ****** ****** *)
fun tpxyz_new(): type
(* ****** ****** *)
fun type_norm(type): type
(* ****** ****** *)
fun
print_type(type): void
fun
fprint_type
(out:FILEref, tp:type): void
(* ****** ****** *)
overload print with print_type
overload fprint with fprint_type
(* ****** ****** *)
(* ****** ****** *)
fun
txyz_solve
(r1: txyz, T2: type): bool
(* ****** ****** *)
fun
type_unify
(tp1: type, tp2: type): bool
(* ****** ****** *)
(* ****** ****** *)
//
typedef tvar = string
typedef topr = string
//
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
| TMfst of (term)
| TMsnd of (term)
| TMtup of (term, term)
//
| TMfix of
  (tvar(*f0*), tvar(*x1*), term)
//
| TMlam2 of
  (tvar, type, term)
| TMfix2 of
  (tvar(*f0*), tvar(*x1*), type, term)
//
where termlst = mylist(term)
//
(* ****** ****** *)

typedef tpctx = mylist(@(tvar, type))

(* ****** ****** *)
fun
print_term(t0:term): void
fun
fprint_term
(out:FILEref, t0:term): void
(* ****** ****** *)
overload print with print_term
overload fprint with fprint_term
(* ****** ****** *)
//
fun
term_type0(term): type
fun
term_type1(term, tpctx): type
fun
term_type1_ck
(t0:term, T0:type, c0:tpctx): void
fun
termlst_type1(termlst, tpctx): typelst
//
(* ****** ****** *)

fun
tpctx_lookup(tpctx, tvar): type

(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/midterm.sats] *)
