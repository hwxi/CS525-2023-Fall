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
|
TMopr(nm, ts) =>
fprint!(out, "TMopr(", nm, ";", ts, ")")
|
TMif0(t1, t2, t3) =>
fprint!(out, "TMif0(", t1, ";", t2, ";", t3, ")")
)
//
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

typedef
valuelst = mylist(value)

(* ****** ****** *)
extern
fun
print_value(v0:value): void
extern
fun
fprint_value
(out:FILEref, v0:value): void
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
VALlam(t1, env) =>>
fprint!(out, "VALbtf(", t1, ";", "...", ")")
)
//
(* ****** ****** *)

extern
fun
term_eval0(term): value
extern
fun
term_eval1(term, envir): value
extern
fun
termlst_eval1
(ts:termlst, e0:envir): valuelst

(* ****** ****** *)

extern
fun
envir_lookup(envir, tvar): value

(* ****** ****** *)
//
implement
term_eval0(t0) =
term_eval1(t0, e0) where
{
  val e0 = mylist_nil(*void*) }
//
(* ****** ****** *)

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
|
VALlam(t1, e1) =>
let
val-TMlam(x1, tt) = t1
in
term_eval1
(tt, mylist_cons((x1, v2), e1))
end
end
//
|
TMopr(nm, ts) =>
let
val vs = termlst_eval1(ts, e0)
in//let
//
case- nm of
| "+" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALint(i1 + i2)
end
| "-" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALint(i1 - i2)
end
| "*" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALint(i1 * i2)
end
| "/" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALint(i1 / i2)
end
| "%" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALint(i1 % i2)
end
//
| "<" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALbtf(i1 < i2)
end
| ">" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALbtf(i1 > i2)
end
| "<=" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALbtf(i1 <= i2)
end
| ">=" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALbtf(i1 >= i2)
end
//
| _(*unsupported*) =>
(
exit(1) ) where
{
val () = println!("term_eval1: t0 = ", t0)
}
//
end // end of [TMopr(nm, ts)]
//
//
|
TMif0(t1, t2, t3) =>
let
val t1 = term_eval1(t1, e0)
in//let
case- t1 of
|
VALbtf(b1) =>
(
  if b1 then term_eval1(t2, e0)
        else term_eval1(t3, e0))
end // let // end of [TMif0(...)]
//
(*
| _(*otherwise*) =>
(
exit(1) ) where
{
val () = println!("term_eval1: t0 = ", t0)
}
*)
) where
{
(*
  val () = println!("term_eval1: t0 = ", t0)
*)
}

(* ****** ****** *)

implement
termlst_eval1(ts, e0) =
(
case+ ts of
|
mylist_nil() => mylist_nil()
|
mylist_cons(t1, ts) =>
mylist_cons
(term_eval1(t1, e0), termlst_eval1(ts, e0))
)

(* ****** ****** *)

implement
envir_lookup
(xvs, x0) =
(
case+ xvs of
|
mylist_nil() => exit(1) where
{
  val () = println!("envir_lookup: x0 = ", x0)
}
|
mylist_cons(xv1, xvs) =>
if x0 = xv1.0 then xv1.1 else envir_lookup(xvs, x0)
)

(* ****** ****** *)

fun
TMlt
( t1: term
, t2: term): term =
TMopr("<", mylist_pair(t1, t2))
fun
TMgt
( t1: term
, t2: term): term =
TMopr(">", mylist_pair(t1, t2))
fun
TMlte
( t1: term
, t2: term): term =
TMopr("<=", mylist_pair(t1, t2))
fun
TMgte
( t1: term
, t2: term): term =
TMopr(">=", mylist_pair(t1, t2))

fun
TMadd
( t1: term
, t2: term): term =
TMopr("+", mylist_pair(t1, t2))
fun
TMsub
( t1: term
, t2: term): term =
TMopr("-", mylist_pair(t1, t2))
fun
TMmul
( t1: term
, t2: term): term =
TMopr("*", mylist_pair(t1, t2))
fun
TMdiv
( t1: term
, t2: term): term =
TMopr("/", mylist_pair(t1, t2))
fun
TMmod
( t1: term
, t2: term): term =
TMopr("%", mylist_pair(t1, t2))

(* ****** ****** *)

(*
Y = λf· (λx· f (x x)) (λx· f (x x))
*)
val Y =
let
val f = TMvar"f"
and x = TMvar"x" in
TMlam("f",
TMapp(
TMlam("x", TMapp(f, TMapp(x, x))),
TMlam("x", TMapp(f, TMapp(x, x))))) end

(* ****** ****** *)

(*
Y' = λf· (λx· f (λy· x x y)) (λx· f (λy· x x y))
*)
val Y' =
let
val f = TMvar"f"
and x = TMvar"x"
and y = TMvar"y" in
TMlam("f",
TMapp(
TMlam("x",
TMapp(f, TMlam("y", TMapp(TMapp(x, x), y)))),
TMlam("x",
TMapp(f, TMlam("y", TMapp(TMapp(x, x), y)))))) end

(* ****** ****** *)

val
TMfact = TMapp(Y', FACT) where
{
val FACT =
let
val f = TMvar"f"
val x = TMvar"x" in
TMlam("f",
TMlam("x",
TMif0(
TMgte(x, TMint(1)),
TMmul(x,
TMapp(f,
TMsub(x, TMint(1)))), TMint(1)))) end
}

(* ****** ****** *)

val
TMfibo = TMapp(Y', FIBO) where
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

val
VALfact5 =
term_eval0(TMapp(TMfact, TMint(5)))
val () = println!("VALfact5 = ", VALfact5)
val
VALfact10 =
term_eval0(TMapp(TMfact, TMint(10)))
val () = println!("VALfact10 = ", VALfact10)

(* ****** ****** *)

val
VALfibo5 =
term_eval0(TMapp(TMfibo, TMint(5)))
val () = println!("VALfibo5 = ", VALfibo5)
val
VALfibo10 =
term_eval0(TMapp(TMfibo, TMint(10)))
val () = println!("VALfibo10 = ", VALfibo10)
val
VALfibo20 =
term_eval0(TMapp(TMfibo, TMint(20)))
val () = println!("VALfibo20 = ", VALfibo20)

(* ****** ****** *)

(* end of [CS525-2022-Fall/lecture/lecture-10-02/lambda1.dats] *)
