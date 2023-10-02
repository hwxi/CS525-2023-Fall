(* ****** ****** *)
#include
"share\
/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../mylib/mylib.dats"
(* ****** ****** *)
implement main0 () = ()
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
| TMopr of (topr, termlst)
//
| TMif0 of (term, term, term)
//
where termlst = mylist(term)
//
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
|
TMopr(nm, ts) =>
fprint!(out, "TMopr(", nm, ";", ts, ")")
|
TMif0(t1, t2, t3) =>
fprint!(out, "TMif0(", t1, ";", t2, ";", t3, ")")
)

(* ****** ****** *)

datatype
value =
//
| VALint of int
| VALbtf of bool
//
| VALlam of (term(*lam*), envir)

where envir = mylist(@(tvar, value))

(* ****** ****** *)

extern
fun
term_eval0(term): value
extern
fun
term_eval1(term, envir): value

(* ****** ****** *)

implement
term_eval0(t0) =
term_eval1(t0, e0) where
{
  val e0 = mylist_nil(*void*)
}

extern
fun
envir_lookup(envir, tvar): value

implement
term_eval1(t0, e0) =
(
case+ t0 of
//
|
TMvar(x0) =>
envir_lookup(e0, x0)
|
TMint(i0) => VALint(i0)
|
TMbtf(b0) => VALbtf(b0)
//
|TMlam _ => VALlam(t0, e0)
//
|TMapp(t1, t2) =>
let
//
val v1 = term_eval1(t1, e0)
val v2 = term_eval1(t2, e0) // call-by-value
//
in
case- v1 of
| VALlam(t1, e1) =>
  let
  val-TMlam(x1, tt) = t1
  in
    term_eval1(tt, mylist_cons((x1, v2), e1))
  end
end
| _(*otherwise*) =>
(
exit(1) ) where
{
val () =
println!("term_eval1: t0 = ", t0)
}
)

(* ****** ****** *)

fun
TMlt
( t1: term
, t2: term): term = TMopr("<", mylist_pair(t1, t2))
fun
TMgt
( t1: term
, t2: term): term = TMopr(">", mylist_pair(t1, t2))
fun
TMlte
( t1: term
, t2: term): term = TMopr("<=", mylist_pair(t1, t2))
fun
TMgte
( t1: term
, t2: term): term = TMopr(">=", mylist_pair(t1, t2))

fun
TMadd
( t1: term
, t2: term): term = TMopr("+", mylist_pair(t1, t2))
fun
TMsub
( t1: term
, t2: term): term = TMopr("-", mylist_pair(t1, t2))
fun
TMmul
( t1: term
, t2: term): term = TMopr("*", mylist_pair(t1, t2))

val Y =
let
val f = TMvar"f"
and x = TMvar"x" in
TMlam("f",
TMapp(
TMlam("x", TMapp(f, TMapp(x, x))),
TMlam("x", TMapp(f, TMapp(x, x))))) end

val
TMfibo = TMapp(Y, FIBO) where
{
val FIBO =
let
val f = TMvar"f"
val x = TMvar"x" in
TMlam("f",
TMlam("x",
TMif0(
TMgte(x, TMint(2)),
TMadd(
TMapp(f, TMsub(x, TMint(2))),
TMapp(f, TMsub(x, TMint(1)))), x))) end
}

(* ****** ****** *)

val VALfibo0 = term_eval0(TMapp(TMfibo, TMint(5)))

(* ****** ****** *)
