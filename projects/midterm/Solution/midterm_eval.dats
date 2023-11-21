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
(* ****** ****** *)
exception EXNstr_get_at
exception EXNoptn_uncons1
(* ****** ****** *)
exception EXNlist_uncons1
exception EXNlist_uncons2
(* ****** ****** *)
exception EXNllist_uncons1
exception EXNllist_uncons2
(* ****** ****** *)
(* ****** ****** *)
//
extern
fun
{a:t@ype}
{b:t@ype}
mylist_map
( xs: mylist(a)
, fopr: a -> b): mylist(b)
//
implement
{a}{b}//tmp
mylist_map(xs, fopr) =
case+ xs of
|
mylist_nil() =>
mylist_nil()
|
mylist_cons(x1, xs) =>
mylist_cons
(fopr(x1), mylist_map(xs, fopr))
//
(* ****** ****** *)
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
|TMvar(x0) =>
envir_lookup(e0, x0)
//
|TMnil() => VALnil()
//
|TMint(i0) => VALint(i0)
|TMbtf(b0) => VALbtf(b0)
|TMchr(c0) => VALchr(c0)
|TMstr(s0) => VALstr(s0)
//
|TMlam _ => VALlam(t0, e0)
|TMfix _ => VALfix(t0, e0)
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
|
VALfix(t1, e1) =>
let
val-
TMfix(f1, x2, tt) = t1
in
term_eval1
(tt, mylist_cons((f1, v1), mylist_cons((x2, v2), e1)))
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
| "=" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALbtf(i1 = i2)
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
| "!=" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALint(i1) = v1 and VALint(i2) = v2
in
  VALbtf(i1 != i2)
end
//
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
(* ****** ****** *)
//
| "ord" =>
let
val-
mylist_cons
( v1 , vs ) = vs
val-
VALchr(c0) = v1 in VALint(char2int0(c0))
end
//
(* ****** ****** *)
//
| "print" =>
let
val-mylist_cons(v1, vs) = vs
in//let
let
val-
VALint(i0) = v1 in print(i0); VALnil() end
end
| "prchr" =>
let
val-mylist_cons(v1, vs) = vs
in//let
let
val-
VALchr(c0) = v1 in print(c0); VALnil() end
end
| "prstr" =>
let
val-mylist_cons(v1, vs) = vs
in//let
let
val-
VALstr(cs) = v1 in print(cs); VALnil() end
end
//
(* ****** ****** *)
//
| "str_len" =>
let
val-mylist_cons(v1, vs) = vs
in
let
val-
VALstr(cs) = v1 in
  VALint(g0u2i(string_length(cs))) end
end
//
| "str_get_at" =>
let
val-mylist_cons(v1, vs) = vs
val-mylist_cons(v2, vs) = vs
val-VALstr(cs) = v1 and VALint(i0) = v2
in//let
let
val i0 = g1ofg0_int(i0)
val cs = g1ofg0_string(cs)
val n0 = g1u2i(string1_length(cs))
in//let
if
i0 < 0
then $raise EXNstr_get_at else
(
if
i0 >= n0
then $raise EXNstr_get_at
else VALchr(string_get_at(cs, i0)) ) end
end // end of [str_get_at]
//
| "str_make_list" =>
let
val-mylist_cons(v1, vs) = vs
in//let
let
val-VALlst(cs) = v1 in
VALstr
(
string_make_mylist
(
mylist_map
( cs
, lam c =>
  let val-VALchr(c) = c in c end)) ) end
end // end of [str_make_list]
//
(* ****** ****** *)
//
| "ref_get" =>
let
val-
mylist_cons(v1, vs) = vs
in
  let val-VALref(vr) = v1 in !vr end
end
| "ref_set" =>
let
val-
mylist_cons(v1, vs) = vs
val-
mylist_cons(v2, vs) = vs
in
let
val-
VALref(vr) = v1 in !vr := v2; VALnil() end
//
end
| "ref_new" =>
let
val-
mylist_cons(v1, vs) = vs in VALref(ref(v1))
end // end of [ref_new]
//
(* ****** ****** *)
(*
list_nil, list_cons
list_nilq, list_consq,
list_uncons1, list_uncons2
*)
| "list_nil" =>
VALlst(mylist_nil())
| "list_cons" =>
let
val-
mylist_cons(v1, vs) = vs
val-
mylist_cons(v2, vs) = vs in
let
val-VALlst(xs) = v2 in
VALlst(mylist_cons(v1, xs)) end end
//
| "list_nilq" =>
let
val-
mylist_cons(v1, vs) = vs in
let
val-
VALlst(xs) = v1 in VALbtf(mylist_nilq(xs))
end
end // end of [list_nilq]
| "list_consq" =>
let
val-
mylist_cons(v1, vs) = vs in
let
val-
VALlst(xs) = v1 in VALbtf(mylist_consq(xs))
end
end // end of [list_consq]
//
| "list_uncons1" =>
let
val-
mylist_cons(v1, vs) = vs in
let
val-
VALlst(xs) = v1 in case+ xs of
| mylist_nil() =>
  $raise EXNlist_uncons1()
| mylist_cons(x1, xs) => x1 end end // let
//
| "list_uncons2" =>
let
val-
mylist_cons(v1, vs) = vs in
let
val-
VALlst(xs) = v1 in case+ xs of
| mylist_nil() =>
  $raise EXNlist_uncons2()
| mylist_cons(x1, xs) => VALlst(xs) end end
//
(* ****** ****** *)
(*
llist_nil, llist_cons
llist_nilq, llist_consq
llist_uncons1, llist_uncons2
*)
| "llist_nil" => VALnil()
| "llist_cons" =>
(
  VALtup(v1, v2) ) where
{
  val-mylist_cons(v1, vs) = vs
  val-mylist_cons(v2, vs) = vs }
//
| "llist_nilq" =>
let
val-
mylist_cons(v1, vs) = vs in
(
case+ v1 of
| VALnil() => VALbtf(true)
| _(*non-VALnil*) => VALbtf(false)) end
| "llist_consq" =>
let
val-
mylist_cons(v1, vs) = vs in
(
case+ v1 of
| VALtup _ => VALbtf(true)
| _(*non-VALtup*) => VALbtf(false)) end
//
| "llist_uncons1" =>
let
val-
mylist_cons(v1, vs) = vs in
let
val-VALtup(v11, v12) = v1 in v11 end end
| "llist_uncons2" =>
let
val-
mylist_cons(v1, vs) = vs in
let
val-VALtup(v11, v12) = v1 in v12 end end
//
(* ****** ****** *)
//
| _(*unsupported*) =>
(
exit(1) ) where
{
  val () = println!("term_eval1: t0 = ", t0) }
//
end // end of [TMopr(nm, ts)]
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
|
TMlet(x1, t1, t2) =>
let
val v1 = term_eval1(t1, e0)
in
  term_eval1
  (t2, mylist_cons((x1, v1), e0))
end
//
|
TMfst(tt) =>
let
val-
VALtup(v1, _) =
term_eval1(tt, e0) in v1 end
|
TMsnd(tt) =>
let
val-
VALtup(_, v2) =
term_eval1(tt, e0) in v2 end
//
|
TMtup(t1, t2) =>
(
  VALtup(v1, v2)) where
{
  val v1 = term_eval1(t1, e0)
  val v2 = term_eval1(t2, e0) }
//
|
TManno(t1, T1) => term_eval1(t1, e0)
//
|
TMlamt
( x1
, Tx, tt) => VALlam(TMlam(x1, tt), e0)
|
TMfixt
( f0, x1
, Tf, tt) => VALlam(TMfix(f0, x1, tt), e0)
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

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_eval.dats] *)
