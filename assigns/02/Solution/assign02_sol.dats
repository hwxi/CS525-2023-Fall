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
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)

#include "./../assign02.dats"

(* ****** ****** *)

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

extern
fun
term_interp(t0: term): term
extern
fun
termlst_interp(ts: termlst): termlst

(* ****** ****** *)

extern
fun
term_subst0 // t0[x0 -> sub]
(t0: term, x0: tvar, sub: term): term
extern
fun
termlst_subst0 // t0[x0 -> sub]
(ts: termlst, x0: tvar, sub: term): termlst

(* ****** ****** *)

implement
term_interp(t0) =
(
case+ t0 of
//
|TMint _ => t0
|TMbtf _ => t0
//
|TMvar _ => t0
//
|TMlam _ => t0
//
(*
// HX: please note
// no evaluagtion under lambda!!!
|
TMlam(x1, t1) =>
TMlam(x1, t1) where
{
val t1 = term_interp(t1)
}
*)
//
|
TMapp(t1, t2) =>
let
val t1 = term_interp(t1)
(*
val t2 = term_interp(t2)
*)
in//let
case+ t1 of
| TMlam(x1, tt) =>
term_interp
(term_subst0(tt, x1, t2))
| _(*non-TMlam*) => TMapp(t1, t2) // type-error
end // let // end of [TMapp]
//
|
TMopr(nm, ts) =>
let
val ts = termlst_interp(ts)
in//let
//
case- nm of
| "+" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMint(i1 + i2)
end
| "-" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMint(i1 - i2)
end
| "*" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMint(i1 * i2)
end
| "<" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMbtf(i1 < i2)
end
| ">" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMbtf(i1 > i2)
end
| "<=" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMbtf(i1 <= i2)
end
| ">=" =>
let
val-mylist_cons(t1, ts) = ts
val-mylist_cons(t2, ts) = ts
val-TMint(i1) = t1 and TMint(i2) = t2
in
  TMbtf(i1 >= i2)
end
//
end // end of [TMopr(nm, ts)]
//
|
TMif0(t1, t2, t3) =>
let
val t1 = term_interp(t1)
in//let
case+ t1 of
|
TMbtf(b1) =>
if b1
then term_interp(t2) else term_interp(t3)
|
_(*non-TMbtf*) =>
TMif0(t1, term_interp(t2), term_interp(t3)) // type-error
end // let // end of [TMif0]
) (* case+ *) // end of [term_interp]

(* ****** ****** *)

implement
termlst_interp(ts) =
(
case+ ts of
|
mylist_nil() =>
mylist_nil()
|
mylist_cons(t1, ts) =>
mylist_cons
(term_interp(t1), termlst_interp(ts))
)

(* ****** ****** *)

implement
term_subst0(t0, x0, sub) =
(
case+ t0 of
//
| TMint _ => t0
| TMbtf _ => t0
//
| TMvar(x1) =>
  if x0 = x1 then sub else t0
//
| TMlam(x1, tt) =>
  if x0 = x1 then t0 else
  TMlam(x1, term_subst0(tt, x0, sub))
//
| TMapp(t1, t2) =>
  TMapp(t1, t2) where
  {
    val t1 = term_subst0(t1, x0, sub)
    val t2 = term_subst0(t2, x0, sub)
  }
//
| TMopr(nm, ts) =>
  TMopr(nm, ts) where
  {
    val ts =
    termlst_subst0(ts, x0, sub)
  }
//
| TMif0(t1, t2, t3) =>
  TMif0(t1, t2, t3) where
  {
    val t1 = term_subst0(t1, x0, sub)
    val t2 = term_subst0(t2, x0, sub)
    val t3 = term_subst0(t3, x0, sub)
  }
//
(*
| _ (* otherwise *) =>
(
assert(false); exit(0)) where
{
  val () = print!("term_subst0: t0 = ", t0)
}
*)
//
) (* end of [term_subst0(t0, x0, sub)] *)

(* ****** ****** *)

implement
termlst_subst0(ts, x0, sub) =
(
case+ ts of
|
mylist_nil() => mylist_nil()
|
mylist_cons(t1, ts) =>
mylist_cons
(term_subst0(t1, x0, sub), termlst_subst0(ts, x0, sub))
) (* end of [termlst_subst0(t0, x0, sub)] *)

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

(* ****** ****** *)

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

(* ****** ****** *)

val Y =
let
val f = TMvar"f"
and x = TMvar"x" in
TMlam("f",
TMapp(
TMlam("x", TMapp(f, TMapp(x, x))),
TMlam("x", TMapp(f, TMapp(x, x))))) end

(* ****** ****** *)

val
TMfibo = TMapp(
Y, 
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
)

(* ****** ****** *)
val () = println!
("TMfibo5 = ", term_interp(TMapp(TMfibo, TMint(5))))
val () = println!
("TMfibo10 = ", term_interp(TMapp(TMfibo, TMint(10))))
(* ****** ****** *)

local
//
val f = TMvar"f"
//
val p = TMvar"p"
//
val x = TMvar"x"
val y = TMvar"y"
val z = TMvar"z"
//
val m = TMvar"m"
and n = TMvar"n"
//
in//local

val K = TMlam("x", TMlam("y", x))
val K' = TMlam("x", TMlam("y", y))

val Church_1 =
TMlam("f", TMlam("x", TMapp(f, x)))
val Church_2 =
TMlam("f", TMlam("x", TMapp(f, TMapp(f, x))))

val Church_tt = K
val Church_ff = K'

fun
Church_suc
(tn: term): term =
TMapp
(
TMlam("n",
TMlam("f", TMlam("x",
TMapp(f, TMapp(TMapp(n, f), x))))), tn)

fun
Church_pair
(tx: term, ty: term): term =
TMapp
(
TMapp
(
TMlam("x", TMlam("y",
TMlam("p", TMapp(TMapp(p, x), y)))), tx), ty)

fun
Church_fst(tp: term): term = TMapp(tp, K)
fun
Church_snd(tp: term): term = TMapp(tp, K')

fun
Church_gtz(tn: term): term =
TMapp
(
TMlam("n",
TMlam("x", TMlam("y",
TMapp(TMapp(n, TMapp(K, x)), y)))), tn)

fun
Church_if0
(t1: term, t2: term, t3: term) = TMapp(TMapp(t1, t2), t3)

fun
Church_pre(tn: term): term =
TMapp
(
TMlam("n",
TMlam("f", TMlam("x",
Church_fst
(TMapp(TMapp(n, F), X))))), tn) where
{
val X = Church_pair(x, x)
val F =
TMlam("p", Church_pair(Church_snd(p), TMapp(f, Church_snd(p))))
}

fun
Church_add
(tm: term, tn: term): term =
TMapp(TMapp(tm, TMlam("n", Church_suc(n))), tn)

fun
Church_sub
(tm: term, tn: term): term =
TMapp(TMapp(tn, TMlam("m", Church_pre(m))), tm)

end // end of [local]

(* ****** ****** *)

fun
Church_numeral(n: int): term =
(
TMlam("f", TMlam("x", helper(n)))) where
{
val f = TMvar"f"
val x = TMvar"x"
fun helper(n: int): term =
if n <= 0 then x else TMapp(f, helper(n-1)) }

(* ****** ****** *)

val
TMsuc = // successor
TMlam("x", TMadd(TMvar"x", TMint(1)))

(* ****** ****** *)

fun
Church_num2int(n: term): term =
term_interp(TMapp(TMapp(n, TMsuc), TMint(0)))

(* ****** ****** *)
val TMnum0 = Church_numeral(0)
val TMnum1 = Church_numeral(1)
val TMnum2 = Church_numeral(2)
val TMnum3 = Church_numeral(3)
val TMnum4 = Church_numeral(4)
val TMnum5 = Church_numeral(5)
val TMnum6 = Church_numeral(6)
(* ****** ****** *)

val () =
println!("pre(0) = ", Church_num2int(Church_pre(TMnum0)))
val () =
println!("pre(5) = ", Church_num2int(Church_pre(TMnum5)))
val () =
println!("add(1,2) = ", Church_num2int(Church_add(TMnum1, TMnum2)))
val () =
println!("add(2,1) = ", Church_num2int(Church_add(TMnum2, TMnum1)))
val () =
println!("sub(1,2) = ", Church_num2int(Church_sub(TMnum1, TMnum2)))
val () =
println!("sub(2,1) = ", Church_num2int(Church_sub(TMnum2, TMnum1)))

(* ****** ****** *)

val () =
println!("fst(1,2) = ", Church_num2int(Church_fst(Church_pair(TMnum1, TMnum2))))
val () =
println!("snd(1,2) = ", Church_num2int(Church_snd(Church_pair(TMnum1, TMnum2))))

(* ****** ****** *)

val
Church_fibo =
TMapp(
Y, 
let
val f = TMvar"f"
val x = TMvar"x" in
TMlam("f",
TMlam("x",
Church_if0
(
Church_gtz
(Church_sub(x, TMnum1)),
Church_add(
TMapp(f, Church_sub(x, TMnum2)),
TMapp(f, Church_sub(x, TMnum1))), x))) end
)

(* ****** ****** *)

val () = println!
("Church_fibo(3) = ", Church_num2int(TMapp(Church_fibo, TMnum3)))
val () = println!
("Church_fibo(5) = ", Church_num2int(TMapp(Church_fibo, TMnum5)))
val () = println!
("Church_fibo(6) = ", Church_num2int(TMapp(Church_fibo, TMnum6)))
val () = println!
("Church_fibo(7) = ", Church_num2int(TMapp(Church_fibo, Church_numeral(7))))
val () = println!
("Church_fibo(8) = ", Church_num2int(TMapp(Church_fibo, Church_numeral(8))))

(* ****** ****** *)

(* end of [assign02_sol.dats] *)
