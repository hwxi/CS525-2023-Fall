(* ****** ****** *)
(*
CS525-2023-Fall: midterm
*)
(* ****** ****** *)
#include
"share\
/atspre_staload.hats"
(* ****** ****** *)
//
#staload
"./../midterm.sats"//opened
//
(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)
implement
main0 = lam () => ()
(* ****** ****** *)
exception TypeError of ()
(* ****** ****** *)
val TPint = TPbas("int")
val TPbtf = TPbas("bool")
val TPstr = TPbas("string")
(* ****** ****** *)
implement
tpxyz_new() =
TPxyz(ref(myoptn_nil()))
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
//
implement
fprint_type
(out, tp) =
(
case+ tp of
|
TPbas(nm) =>
fprint!(out, "TPbas(", nm, ")")
//
|
TPxyz(r0) =>
fprint!(out, "TPxyz(", !r0, ")")
//
|
TPfun(tp1, tp2) =>
fprint!(out, "TPfun(", tp1, ";", tp2, ")")
|
TPtup(tp1, tp2) =>
fprint!(out, "TPtup(", tp1, ";", tp2, ")")
)
//
(* ****** ****** *)
//
implement
type_norm(T0) =
(
case+ T0 of
|
TPxyz(r1) =>
(
case+ !r1 of
|
myoptn_cons
(    T1    ) =>
type_norm(T1) // chasing
|
_(*unsolved*) => (  T0  ))
|
_(* non-TPxyz *) => ( T0 ))
//
(* ****** ****** *)

implement
txyz_solve(r1, T2) =
let
//
fun
occurs(T0: type): bool =
(
case+ T0 of
|
TPbas _ => false
|
TPxyz(r2) => (r1 = r2)
//
|
TPfun(T1, T2) =>
occurs(T1) || occurs(T2)
|
TPtup(T1, T2) =>
occurs(T1) || occurs(T2)
//
) where
{
  val T0 = type_norm(T0) }
//
in//let
  if
  occurs(T2)
  then false else
  (!r1 := myoptn_cons(T2); true)
end // end of [txyz_solve(r1, T2)]

(* ****** ****** *)

#symload
unify with type_unify

(* ****** ****** *)
//
implement
type_unify
( T1, T2 ) =
let
//
val T1 = type_norm(T1)
val T2 = type_norm(T2)
//
in
  f0_helper1(T1, T2) end
where
{
//
fun
f0_helper1
( T1: type
, T2: type): bool =
(
case+ T1 of
|
TPxyz(r1) =>
(
case+ T2 of
|
TPxyz(r2) =>
if
(r1 = r2)
then true else txyz_solve(r1, T2)
|
_(*non-TPxyz*) => txyz_solve(r1, T2))
|
_(*non-TPxyz*) =>
(
case+ T2 of
| TPxyz(r2) => txyz_solve(r2, T1)
| _(*non-TPxyz*) => f0_helper2(T1, T2))
)
//
and
f0_helper2
( T1: type
, T2: type): bool =
(
case+ T1 of
|
TPbas(nm1) =>
(case+ T2 of
//
|
TPbas(nm2) =>
(nm1 = nm2) | _ => false)
//
|
TPxyz( _ ) =>
exit(1) where
{
val () =
println!
("type_unify: f0_helper2: T1 = ", T1) }
//
|
TPfun(T11, T12) =>
(case+ T2 of
|
TPfun(T21, T22) =>
(
unify(T11, T21)
&&
unify(T12, T22)) | _(*non-TPtup*) => false)
//
|
TPtup(T11, T12) =>
(case+ T2 of
|
TPtup(T21, T22) =>
(
unify(T11, T21)
&&
unify(T12, T22)) | _(*non-TPtup*) => false)
//
)
}
//
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
TMvar(x0) =>
fprint!(out, "TMvar(", x0, ")")
|
TMlam(x0, t1) =>
fprint!(out, "TMlam(", x0, ";", t1, ")")
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
TMlet(x1, t1, t2) =>
fprint!
(out, "TMlet(", x1, ";", t1, ";", t2, ")")
//
|
TMfst(tt) =>
fprint!(out, "TMfst(", tt, ")")
|
TMsnd(tt) =>
fprint!(out, "TMsnd(", tt, ")")
|
TMtup(t1, t2) =>
(
 fprint!(out, "TMtup(", t1, ";", t2, ")"))
//
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

implement
term_type0
 (  t0  ) =
term_type1
(t0, mylist_nil(*void*))

(* ****** ****** *)
//
fun
type_funize
(T0: type): void =
(
case+ T0 of
//
|
TPxyz(r1) =>
let
val T1 = tpxyz_new()
val T2 = tpxyz_new()
in
!r1 :=
myoptn_cons(TPfun(T1, T2))
end
//
| TPfun _ => ()
| _(*else*) =>
  $raise TypeError()
) where
{
  val T0 = type_norm(T0) }
//
(* ****** ****** *)
//
fun
type_tupize
(T0: type): void =
(
case+ T0 of
//
|
TPxyz(r1) =>
let
val T1 = tpxyz_new()
val T2 = tpxyz_new()
in
!r1 :=
myoptn_cons(TPtup(T1, T2))
end
//
| TPtup _ => ()
| _(*else*) =>
  $raise TypeError()
) where
{
  val T0 = type_norm(T0) }
//
(* ****** ****** *)

implement
term_type1
 (t0, c0) =
(
case+ t0 of
//
|
TMint(i0) => TPint
|
TMbtf(b0) => TPbtf
|
TMstr(s0) => TPstr
//
|
TMvar(x0) =>
tpctx_lookup(c0, x0)
//
|
TMlam(x0, tt) =>
let
val Tx = tpxyz_new()
val c1 =
mylist_cons((x0, Tx), c0)
in//let
  TPfun(Tx, Tt) where
{
  val Tt = term_type1(tt, c1)
}
end//end-of-[TMlam2(x0,Tx,tt)]
//
|
TMapp(t1, t2) =>
(
  T12 ) where
{
val T1 =
term_type1(t1, c0)
//
val () =
type_funize(T1)
val T1 = type_norm(T1)
//
val-
TPfun(T11, T12) = T1
val () =
term_type1_ck(t2, T11, c0)
}
//
|
TMopr(nm, ts) =>
(
case+ nm of
//
| ">" => TPbtf where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "<" => TPbtf where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| ">=" => TPbtf where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "<=" => TPbtf where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "+" => TPint where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "-" => TPint where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "*" => TPint where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "/" => TPint where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| "%" => TPint where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPint, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
| _(*unsupported*) =>
(
exit(1) ) where
{
val () =
println!
("term_type1:TMopr:t0 = ", t0)
}
//
) (*case+*)//end-of-[TMopr(nm,ts)]
//
|
TMif0
(t1, t2, t3) =>
(
  T2 ) where
{
val () =
term_type1_ck
(t1, TPbtf, c0)
val T2 =
term_type1(t2, c0)
val () =
term_type1_ck(t3, T2, c0)
}
//
|TMfst(tt) =>
let
//
val TT =
term_type1(tt, c0)
//
val () =
type_tupize(TT)
val TT = type_norm(TT)
//
val-
TPtup(T1, _) = TT in T1 end
//
|TMsnd(tt) =>
let
//
val TT =
term_type1(tt, c0)
//
val () =
type_tupize(TT)
val TT = type_norm(TT)
//
val-
TPtup(_, T2) = TT in T2 end
//
|
TMtup
(t1, t2) =>
TPtup(T1, T2) where
{
val T1 = term_type1(t1, c0)
val T2 = term_type1(t2, c0) }
//
|
TMlet
(x1, t1, t2) =>
(
term_type1(t2, c1)) where
{ val T1 =
  term_type1(t1, c0)
  val c1 =
  mylist_cons((x1, T1), c0) }
//
|
TMfix
(f0, x0, tt) =>
let
//
val Tx = tpxyz_new()
val Ty = tpxyz_new()
val Tf = TPfun(Tx, Ty)
//
val c1 =
mylist_cons((x0, Tx), c0)
val c2 =
mylist_cons((f0, Tf), c1)
in//let
  Tf where
{
  val () =
  term_type1_ck(tt, Ty, c2)
}
end//end-of-[TMfix(f0,x0,tt)]
//
|
TMlam2
(x0, Tx, tt) =>
let
val c1 =
mylist_cons((x0, Tx), c0)
in//let
  TPfun(Tx, Tt) where
{
  val Tt = term_type1(tt, c1)
}
end//end-of-[TMlam2(x0,Tx,tt)]
//
|
TMfix2
(f0, x0, Tf, tt) =>
let
val-
TPfun(Tx, Ty) = Tf
val c1 =
mylist_cons((x0, Tx), c0)
val c2 =
mylist_cons((f0, Tf), c1)
in//let
  Tf where
{
  val () =
  term_type1_ck(tt, Ty, c2)
}
end//end-of-[TMfix2(x0,Tx,tt)]
//
(*
| _(*unsupported*) =>
(
exit(1) ) where
{
val () =
println!("term_type1: t0 = ", t0)
}
*)
//
) (* end-of-[term_type1(t0, c0)] *)

(* ****** ****** *)

implement
tpctx_lookup
(xts, x0) =
(
case+ xts of
|
mylist_nil() => exit(1) where
{
  val () =
  println!("tpctx_lookup: x0 = ", x0)
}
|
mylist_cons(xt1, xts) =>
if x0 = xt1.0 then xt1.1 else tpctx_lookup(xts, x0)
)

(* ****** ****** *)

implement
term_type1_ck
(t0, Tt, ctx) =
let
val res =
type_unify(T0, Tt)
in
if res then () else $raise TypeError()
end where
{
val T0 = term_type1(t0, ctx)
(*
val () = println!("term_type1_ck: t0 = ", t0)
val () = println!("term_type1_ck: T0 = ", T0)
val () = println!("term_type1_ck: Tt = ", Tt)
*)
}

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

val
TMfact =
let
val f = TMvar"f"
val x = TMvar"x" in
TMfix("f", "x",
TMif0(
TMlte(x, TMint(0)),
TMint(1),
TMmul(
x, TMapp(f, TMsub(x, TMint(1)))))) end

(* ****** ****** *)

val
TMfibo =
let
val f = TMvar"f"
val x = TMvar"x" in
TMfix("f", "x",
TMif0(
TMgte(x, TMint(2)),
TMadd(
TMapp(f, TMsub(x, TMint(2))),
TMapp(f, TMsub(x, TMint(1)))), x)) end

(* ****** ****** *)

val () =
try
let
val
TPomega =
term_type0
(TMlam("x", TMapp(TMvar"x", TMvar"x")))
in//let
println!
("TPomega = ", TPomega)
end with ~TypeError() => println!("TPomega: type error!")

(* ****** ****** *)

val () =
try
let
val
TPfact =
term_type0(TMfact)
in
println!("TPfact = ", TPfact)
end with ~TypeError() => println!("TPfact: type error!")

(* ****** ****** *)

val () =
try
let
val
TPfibo =
term_type0(TMfibo)
in
println!
("TPfibo = ", TPfibo)
end with ~TypeError() => println!("TPfibo: type error!")

(* ****** ****** *)

val
TMfact2 =
let
val f = TMvar"f"
val x = TMvar"x"
//
val ir = TMvar"ir"
val i0 = TMfst(ir)
val i1 = TMvar"i1"
val r0 = TMsnd(ir) in
//
TMlam("x",
TMapp(
TMfix("f", "ir",
TMif0(
TMgte(i0, x), r0,
TMlet("i1",
TMadd(i0, TMint(1)), TMapp(f, TMtup(i1, TMmul(i1, r0)))))),
TMtup(TMint(0), TMint(1))))
end // end of [TMfact2]

(* ****** ****** *)

val () =
try
let
val
TPfact2 =
term_type0(TMfact2)
in
println!("TPfact2 = ", TPfact2)
end with ~TypeError() => println!("TPfact2: type error!")

(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_sol.dats] *)
