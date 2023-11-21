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
#staload
"./../midterm_lib0.sats"//opened
//
(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)
val TPnil = TPbas("nil")
val TPint = TPbas("int")
val TPchr = TPbas("char")
val TPbtf = TPbas("bool")
val TPstr = TPbas("string")
(* ****** ****** *)
val TMtrue = TMbtf(true)
val TMfalse = TMbtf(false)
(* ****** ****** *)
fun
TMprchr(t1: term) =
TMopr("prchr", mylist_sing(t1))
fun
TMprint(t1: term) =
TMopr("print", mylist_sing(t1))
fun
TMprstr(t1: term) =
TMopr("prstr", mylist_sing(t1))
(* ****** ****** *)
//
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
TMeq
( t1: term
, t2: term): term =
TMopr("=", mylist_pair(t1, t2))
//
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
TMneq
( t1: term
, t2: term): term =
TMopr("!=", mylist_pair(t1, t2))
//
(* ****** ****** *)
//
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
//
(* ****** ****** *)
fun
TMsuc
(t1: term): term = TMadd(t1, TMint(1))
fun
TMpre
(t1: term): term = TMsub(t1, TMint(1))
(* ****** ****** *)
fun
TMstr_len
(cs: term): term =
TMopr("str_len", mylist_sing(cs))
fun
TMstr_get_at
(cs: term, i0: term): term =
TMopr("str_get_at", mylist_pair(cs, i0))
(* ****** ****** *)
//
fun
TMseq
( t1: term
, t2: term): term =
TMlet("", TManno(t1, TPnil), t2)
fun
TMseq2
( t1: term
, t2: term): term = TMseq(t1, t2)
fun
TMseq3
( t1: term
, t2: term
, t3: term): term = TMseq(t1, TMseq(t2, t3))
//
(* ****** ****** *)
fun
TMlazy
(t1: term): term =
TMlam("", t1)(*thunk*)
fun
TMeval
(t1: term): term =
TMapp(t1(*thunk*), TMnil)
(* ****** ****** *)
fun
TMignr // ignored
(t1: term): term =
TMlet("", t1, TMnil(*con*))
(* ****** ****** *)
//
(*
fun
int_forall
( n0: int
, test: (int) -> bool): bool =
(
fix loop(i0) =>
if i0 < n0
then
(if test(i0)
then loop(i0+1) else false) else true)(0)
*)
//
val
TMint_forall =
let
val n0 = TMvar"n0"
val i0 = TMvar"i0"
val test = TMvar"test"
val loop = TMvar"loop"
in//let
TMlamt("n0", TPint,
TMlamt("test", TPfun(TPint, TPbtf),
  TMapp(
  TMfix("loop", "i0", TMif0(TMlt(i0, n0), TMif0(TMapp(test, i0), TMapp(loop, TMsuc(i0)), TMfalse), TMtrue)), TMint(0))))
end // end-of-let // end of [TMint_forall]
//
(* ****** ****** *)

val
TMstr_forall =
let
val cs = TMvar"cs"
val i0 = TMvar"i0"
val test = TMvar"test"
in//let
TMlamt(
"cs", TPstr,
TMlamt(
"test", TPfun(TPchr, TPbtf),
TMapp(TMapp(
TMint_forall, TMstr_len(cs)), TMlam("i0", TMapp(test, TMstr_get_at(cs, i0))))))
end // end-of-let // end of [TMstr_forall]

(* ****** ****** *)

val
TMforall_foreach =
let
val x0 = TMvar"x0"
val xs = TMvar"xs"
val work = TMvar"work"
val forall = TMvar"forall" in
TMlam("forall",
TMlam("xs", TMlam("work",
TMignr(TMapp(TMapp(forall, xs), TMlam("x0", TMseq(TMapp(work, x0), TMtrue)))))))
end // end-of-let // end of [TMforall_foreach]

(* ****** ****** *)

val
TMint_foreach = TMapp(TMforall_foreach, TMint_forall)
val
TMstr_foreach = TMapp(TMforall_foreach, TMstr_forall)

(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_lib0.dats] *)
