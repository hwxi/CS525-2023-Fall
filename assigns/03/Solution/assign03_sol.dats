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

#include "./../assign03.dats"

(* ****** ****** *)

implement main0((*void*)) = ()

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
TMtup(t1, t2) =>
VALtup(v1, v2) where
{
  val v1 = term_eval1(t1, e0)
  val v2 = term_eval1(t2, e0)
}
//
|TMfst(tt) =>
(
case- vv of
| VALtup(v1, v2) => v1) where
{
  val vv = term_eval1(tt, e0)
}
//
|TMsnd(tt) =>
(
case- vv of
| VALtup(v1, v2) => v2) where
{
  val vv = term_eval1(tt, e0)
}
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
(* ****** ****** *)
(* ****** ****** *)

fun
TMlet_
( x0: tvar
, t1: term
, t2: term): term =
TMapp(TMlam(x0, t2), t1)

(* ****** ****** *)
fun
TMlam2_
( x1: tvar
, x2: tvar
, tt: term): term =
TMlam(x1, TMlam(x2, tt))
(* ****** ****** *)
fun
TMapp2_
( f1: term
, t1: term
, t2: term): term =
TMapp(TMapp(f1, t1), t2)
(* ****** ****** *)
fun
TMlam3_
( x1: tvar
, x2: tvar
, x3: tvar
, tt: term): term =
TMlam
(x1, TMlam2_(x2, x3, tt))
(* ****** ****** *)
//
val I =
TMlam("x", TMvar"x")
//
(* ****** ****** *)
//
fun
int2term
(i0: int): term =
let
val x = TMvar"x"
val f = TMvar"f"
fun
auxmain(i0: int): term =
if i0 <= 0
then x
else TMapp(f, auxmain(i0-1))
in
  TMlam2_("f", "x", auxmain(i0))
end
//
fun
btf2term
(b0: bool): term =
if (b0)
then TMlam2_("x", "y", TMvar"x")
else TMlam2_("x", "y", TMvar"y")
//
fun
TMint_(i:int): term = int2term(i)
fun
TMbtf_(b:bool): term = btf2term(b)
//
(* ****** ****** *)
//
fun
TMfst_(tt: term): term =
TMapp
(tt, TMlam2_("x", "y", TMvar"x"))
fun
TMsnd_(tt: term): term =
TMapp
(tt, TMlam2_("x", "y", TMvar"y"))
fun
TMtup_
(t1: term, t2: term): term =
TMapp2_
(
TMlam3_
(
"x", "y", "p",
TMapp2_
(TMvar"p", TMvar"x", TMvar"y")), t1, t2)
//
(* ****** ****** *)

val
TMtru_ =
TMlam2_("x", "y", TMvar"x")
val
TMfal_ =
TMlam2_("x", "y", TMvar"y")

fun
TMnot_(tb: term): term =
TMapp2_(tb, TMfal_, TMtru_)

(* ****** ****** *)

fun
TMif0_
( t1: term
, t2: term
, t3: term): term =
TMapp
(TMapp2_(t1, TMlam("", t2), TMlam("", t3)), I)

(* ****** ****** *)

fun
TMadd_
( t1: term
, t2: term): term =
TMapp2_(opr, t1, t2) where
{
val opr =
TMlam2_
(
"m", "n"
,
TMlam2_
( "f", "x"
, TMapp2_
  ( TMvar"m", TMvar"f"
  , TMapp2_(TMvar"n", TMvar"f", TMvar"x"))))
}

fun
TMmul_
( t1: term
, t2: term): term =
TMapp2_(opr, t1, t2) where
{
val opr =
TMlam2_
( "m", "n"
, TMlam2_
  ( "f", "x"
  , TMapp2_
    ( TMvar"m"
    , TMapp(TMvar"n", TMvar"f"), TMvar"x")))
}

(* ****** ****** *)

fun
TMpre_(tt: term): term =
TMapp
(
TMlam("n",
TMlam("f", TMlam("x",
TMfst_
(TMapp(TMapp(n, F), X))))), tt) where
{
val x = TMvar"x"
val f = TMvar"f"
val n = TMvar"n"
val p = TMvar"p"
val X = TMtup_(x, x)
val F =
TMlam
( "p"
, TMlet_
  ("x",TMsnd_(p),TMtup_(x,TMapp(f,x))))
}

fun
TMsub_
(tm: term, tn: term): term =
let
val m = TMvar"m" in
TMapp(TMapp(tn, TMlam("m", TMpre_(m))), tm)
end

(* ****** ****** *)

fun
TMisz_
( tn: term ): term =
let
val x = TMvar"x"
val y = TMvar"y"
val n = TMvar"n" in
TMapp
(
TMlam("n",
TMlam("x", TMlam("y",
TMapp(TMapp(n, TMlam("", y)), x)))), tn) end

(* ****** ****** *)

fun
TMgtz_
( tn: term ): term =
let
val x = TMvar"x"
val y = TMvar"y"
val n = TMvar"n" in
TMapp
(
TMlam("n",
TMlam("x", TMlam("y",
TMapp
(TMapp(n, TMlam("", x)), y)))), tn) end

fun
TMlt_
( t1: term
, t2: term): term = TMgtz_(TMsub_(t2, t1))
fun
TMgt_
( t1: term
, t2: term): term = TMgtz_(TMsub_(t1, t2))

fun
TMgte_
( t1: term
, t2: term): term = TMisz_(TMsub_(t2, t1))
fun
TMlte_
( t1: term
, t2: term): term = TMisz_(TMsub_(t1, t2))

(* ****** ****** *)
//
fun
TMdiv
( t1: term
, t2: term): term =
TMopr("/", mylist_pair(t1,t2))
//
(* ****** ****** *)
//
(*
fun
TMmod_
( t1: term
, t2: term): term =
(
TMapp2_(opr, t1, t2)) where
{
val f = TMvar"f"
val m = TMvar"m" and n = TMvar"n"
(*
val opr =
TMlam2_("m", "n", TMsub(m, TMmul(TMdiv(m, n), n)))
*)
val opr = TMapp(Y', TMlam2_("f", "m", TMlam("n", TMif0_(TMgte_(m, n), TMapp2_(f, TMsub_(m, n), n), m))))
}
*)
//
fun
TMmod_
( t1: term
, t2: term): term =
(
TMapp2_(opr, t1, t2)) where
{
val opr =
TMlam("m",TMlam("n",TMapp(TMapp(TMapp(TMlam("f",TMapp(TMlam("x",TMapp(TMvar("f"),TMlam("y",TMapp(TMapp(TMvar("x"),TMvar("x")),TMvar("y"))))),TMlam("x",TMapp(TMvar("f"),TMlam("y",TMapp(TMapp(TMvar("x"),TMvar("x")),TMvar("y"))))))),TMlam("f",TMlam("m",TMlam("n",TMapp(TMapp(TMapp(TMapp(TMlam("n",TMlam("x",TMlam("y",TMapp(TMapp(TMvar("n"),TMlam("",TMvar("y"))),TMvar("x"))))),TMapp(TMapp(TMvar("m"),TMlam("m",TMapp(TMlam("n",TMlam("f",TMlam("x",TMapp(TMapp(TMapp(TMvar("n"),TMlam("p",TMapp(TMlam("x",TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMapp(TMvar("f"),TMvar("x")))),TMapp(TMvar("p"),TMlam("x",TMlam("y",TMvar("y"))))))),TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMvar("x"))),TMlam("x",TMlam("y",TMvar("x"))))))),TMvar("m")))),TMvar("n"))),TMlam("",TMapp(TMapp(TMvar("f"),TMapp(TMapp(TMvar("n"),TMlam("m",TMapp(TMlam("n",TMlam("f",TMlam("x",TMapp(TMapp(TMapp(TMvar("n"),TMlam("p",TMapp(TMlam("x",TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMapp(TMvar("f"),TMvar("x")))),TMapp(TMvar("p"),TMlam("x",TMlam("y",TMvar("y"))))))),TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMvar("x"))),TMlam("x",TMlam("y",TMvar("x"))))))),TMvar("m")))),TMvar("m"))),TMvar("n")))),TMlam("",TMvar("m"))),TMlam("x",TMvar("x"))))))),TMvar("m")),TMvar("n"))))
}
//
(* ****** ****** *)
//
fun
TMdiv_
( t1: term
, t2: term): term =
(
TMapp2_(opr, t1, t2)) where
{
val f = TMvar"f"
val m = TMvar"m" and n = TMvar"n"
val opr = TMapp(Y', TMlam2_("f", "m", TMlam("n", TMif0_(TMgte_(m, n), TMadd_(TMapp2_(f, TMsub_(m, n), n), TMint_(1)), TMint_(0)))))
}
//
(*
fun
TMdiv_
( t1: term
, t2: term): term =
(
TMapp2_(opr, t1, t2)) where
{
val opr =
TMlam("m",TMlam("n",TMapp(TMapp(TMapp(TMlam("f",TMapp(TMlam("x",TMapp(TMvar("f"),TMlam("y",TMapp(TMapp(TMvar("x"),TMvar("x")),TMvar("y"))))),TMlam("x",TMapp(TMvar("f"),TMlam("y",TMapp(TMapp(TMvar("x"),TMvar("x")),TMvar("y"))))))),TMlam("f",TMlam("m",TMlam("n",TMapp(TMapp(TMapp(TMapp(TMlam("n",TMlam("x",TMlam("y",TMapp(TMapp(TMvar("n"),TMlam("",TMvar("y"))),TMvar("x"))))),TMapp(TMapp(TMvar("m"),TMlam("m",TMapp(TMlam("n",TMlam("f",TMlam("x",TMapp(TMapp(TMapp(TMvar("n"),TMlam("p",TMapp(TMlam("x",TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMapp(TMvar("f"),TMvar("x")))),TMapp(TMvar("p"),TMlam("x",TMlam("y",TMvar("y"))))))),TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMvar("x"))),TMlam("x",TMlam("y",TMvar("x"))))))),TMvar("m")))),TMvar("n"))),TMlam("",TMapp(TMapp(TMlam("m",TMlam("n",TMlam("f",TMlam("x",TMapp(TMapp(TMvar("m"),TMvar("f")),TMapp(TMapp(TMvar("n"),TMvar("f")),TMvar("x"))))))),TMapp(TMapp(TMvar("f"),TMapp(TMapp(TMvar("n"),TMlam("m",TMapp(TMlam("n",TMlam("f",TMlam("x",TMapp(TMapp(TMapp(TMvar("n"),TMlam("p",TMapp(TMlam("x",TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMapp(TMvar("f"),TMvar("x")))),TMapp(TMvar("p"),TMlam("x",TMlam("y",TMvar("y"))))))),TMapp(TMapp(TMlam("x",TMlam("y",TMlam("p",TMapp(TMapp(TMvar("p"),TMvar("x")),TMvar("y"))))),TMvar("x")),TMvar("x"))),TMlam("x",TMlam("y",TMvar("x"))))))),TMvar("m")))),TMvar("m"))),TMvar("n"))),TMlam("f",TMlam("x",TMapp(TMvar("f"),TMvar("x"))))))),TMlam("",TMlam("f",TMlam("x",TMvar("x"))))),TMlam("x",TMvar("x"))))))),TMvar("m")),TMvar("n"))))
}
*)
//
(* ****** ****** *)

implement
assign03_compile
  ( t0 ) =
(
  compile(t0)) where
{
fun
compile
(t0: term): term =
(
//
case+ t0 of
//
|TMint _ => f0_int(t0)
|TMbtf _ => f0_btf(t0)
//
|TMvar _ => f0_var(t0)
|TMlam _ => f0_lam(t0)
|TMapp _ => f0_app(t0)
//
|TMtup _ => f0_tup(t0)
|TMfst _ => f0_fst(t0)
|TMsnd _ => f0_snd(t0)
//
|TMif0 _ => f0_if0(t0)
//
|TMopr _ => f0_opr(t0)
//
) where
{
//
fun
f0_int
(t0: term): term =
let
val-
TMint(i0) = t0 in int2term(i0) end
//
fun
f0_btf
(t0: term): term =
let
val-
TMbtf(i0) = t0 in btf2term(i0) end
//
fun
f0_var
(t0: term): term =
(
  let val-TMvar(x0) = t0 in t0 end)
//
fun
f0_lam
(t0: term): term =
let
val-
TMlam(x0, tt) = t0
val tt = compile(tt) in TMlam(x0, tt)
end
//
fun
f0_app
(t0: term): term =
let
val-
TMapp(t1, t2) = t0
val t1 = compile(t1)
val t2 = compile(t2) in TMapp(t1, t2)
end
//
fun
f0_tup
(t0: term): term =
let
val-
TMtup(t1, t2) = t0
val t1 = compile(t1)
val t2 = compile(t2) in TMtup_(t1, t2)
end
//
fun
f0_fst
(t0: term): term =
let
val-TMfst(tt) = t0
val tt = compile(tt) in TMfst_(tt) end
fun
f0_snd
(t0: term): term =
let
val-TMfst(tt) = t0
val tt = compile(tt) in TMsnd_(tt) end
//
fun
f0_if0
(t0: term): term =
let
val-
TMif0(t1, t2, t3) = t0
val t1 = compile(t1)
val t2 = compile(t2)
val t3 = compile(t3) in TMif0_(t1,t2,t3)
end
//
fun
f0_opr
(t0: term): term =
let
val-TMopr(nm, ts) = t0
in
case- nm of
//
| "+" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMadd_(t1, t2)
end
//
| "-" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMsub_(t1, t2)
end
//
| "*" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMmul_(t1, t2)
end
//
| "/" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMdiv_(t1, t2)
end
//
| "%" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMmod_(t1, t2)
end
//
| "<" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMlt_(t1, t2)
end
| ">" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMgt_(t1, t2)
end
//
| "<=" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMlte_(t1, t2)
end
| ">=" =>
let
val-
mylist_cons
( t1 , ts ) = ts
val-
mylist_cons
( t2 , ts ) = ts
val t1 = compile(t1)
val t2 = compile(t2) in TMgte_(t1, t2)
end
//
end // end of [f0_opr(t0)]
} (*where*) // end of [compile(t0)]
//
val () = println!("compile: t0 = ", t0)
//
} (*where*) // end of [assign03_compile(t0)]

(* ****** ****** *)
val () =
assign03_compile_test1((*void*))
val () =
assign03_compile_test2((*void*))
(* ****** ****** *)

(*
//
// HX: real 1m29.842s
//
val () =
let
val
Church_fact8 =
assign03_compile
(TMapp(TMfact, TMint(8)))
//
val x =
TMint(0)
val f =
TMlam("n", TMadd(TMvar"n", TMint(1)))
//
in
println!
( "Church_fact8 = "
, term_eval0(TMapp2_(Church_fact8,f,x)))
end
*)

(* ****** ****** *)
//
// HX: real 0m7.178s
//
val () =
let
val
Church_fibo20 =
assign03_compile
(TMapp(TMfibo, TMint(20)))
//
val x =
TMint(0)
val f =
TMlam("n", TMadd(TMvar"n", TMint(1)))
//
in
println!
( "Church_fibo20 = "
, term_eval0(TMapp2_(Church_fibo20,f,x)))
end
//
(* ****** ****** *)

val () =
let
//
val m = TMvar"m"
val n = TMvar"n"
//
val
Church_mod =
assign03_compile
(
TMlam2_
(
"m", "n",
TMopr("%", mylist_pair(m, n))))
//
in//let
println!("Church_mod = ", Church_mod)
end

val () =
let
//
val
Church_101mod8 =
assign03_compile
(
TMopr
( "%"
, mylist_pair(TMint(101),TMint(8))))
//
val x =
TMint(0)
val f =
TMlam("n", TMadd(TMvar"n", TMint(1)))
//
in//let
println!
( "Church_101mod8 = "
, term_eval0(TMapp2_(Church_101mod8,f,x)))
end

(* ****** ****** *)

val () =
let
//
val m = TMvar"m"
val n = TMvar"n"
//
val
Church_div =
assign03_compile
(
TMlam2_
(
"m", "n",
TMopr("/", mylist_pair(m, n))))
//
in//let
println!("Church_div = ", Church_div)
end

val () =
let
//
val
Church_100div4 =
assign03_compile
(
TMopr
( "/"
, mylist_pair(TMint(100),TMint(4))))
//
val x =
TMint(0)
val f =
TMlam("n", TMadd(TMvar"n", TMint(1)))
//
in//let
println!
( "Church_100div4 = "
, term_eval0(TMapp2_(Church_100div4,f,x)))
end

(* ****** ****** *)

(* end of [CS525-2023-Fall/assign03_sol.dats] *)
