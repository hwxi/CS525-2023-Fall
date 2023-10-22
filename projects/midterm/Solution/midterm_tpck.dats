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
exception TypeError of ()
(* ****** ****** *)
val TPnil = TPbas("nil")
val TPint = TPbas("int")
val TPbtf = TPbas("bool")
val TPchr = TPbas("char")
val TPstr = TPbas("string")
(* ****** ****** *)
implement
tpxyz_new() =
TPxyz(ref(myoptn_nil()))
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
TPref(T1) => occurs(T1)
|
TPlazy(T1) => occurs(T1)
//
|
TPlist(T1) => occurs(T1)
|
TPllist(T1) => occurs(T1)
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
TPref(T10) =>
(
case+ T2 of
|
TPref(T20) =>
unify(T10, T20) | _(*non-TPref*) => false)
|
TPlazy(T10) =>
(
case+ T2 of
|
TPlazy(T20) =>
unify(T10, T20) | _(*non-TPlazy*) => false)
//
|
TPlist(T10) =>
(
case+ T2 of
|
TPlist(T20) =>
unify(T10, T20) | _(*non-TPlist*) => false)
|
TPllist(T10) =>
(
case+ T2 of
|
TPllist(T20) =>
unify(T10, T20) | _(*non-TPllist*) => false)
//
|
TPfun(T11, T12) =>
(case+ T2 of
|
TPfun(T21, T22) =>
(unify(T11, T21)
 &&
 unify(T12, T22)) | _(*non-TPtup*) => false)
//
|
TPtup(T11, T12) =>
(case+ T2 of
|
TPtup(T21, T22) =>
(unify(T11, T21)
 &&
 unify(T12, T22)) | _(*non-TPtup*) => false)
//
)
} (*where*) // end of [ type_unify(T1, T2) ]
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
TMnil() => TPnil(*val*)
//
|
TMint(i0) => TPint(*val*)
|
TMbtf(b0) => TPbtf(*val*)
|
TMchr(c0) => TPchr(*val*)
|
TMstr(s0) => TPstr(*val*)
//
|
TMvar(x0) =>
(
  tpctx_lookup(c0, x0))
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
(* ****** ****** *)
//
|
"str_len" => TPint where
{
val-
mylist_cons(t1, ts) = ts
val () =
term_type1_ck(t1, TPstr, c0) }
//
|
"str_get_at" => TPchr where
{
val-
mylist_cons(t1, ts) = ts
val-
mylist_cons(t2, ts) = ts
val () =
term_type1_ck(t1, TPstr, c0)
val () =
term_type1_ck(t2, TPint, c0) }
//
(* ****** ****** *)
//
| _(*unsupported*) =>
(
exit(1) ) where
{
val () =
println!("term_type1:TMopr:t0 = ", t0)
}
//
) (*case+*) // end-of-[ TMopr(nm, ts) ]
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
TManno(t1, T1) =>
(
term_type1_ck(t1,T1,c0); T1)
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
  val Tt = term_type1(tt, c1) }
end // let // end-of-[TMlam2(x0,Tx,tt)]
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
end // let // end-of-[TMfix2(x0,Tx,tt)]
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
) (*case+*) // end-of-[term_type1(t0, c0)]

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
} (*where*) // end of [term_type1_ck(t0, Tt, ctx)]

(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_tpck.dats] *)
