(* ****** ****** *)
(*
CS525-2023-Fall: midterm
*)
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
(* ****** ****** *)
//
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
//
(* ****** ****** *)
//
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
//
(* ****** ****** *)
//
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
TMadd
(i0, TMint(1)), TMapp(f, TMtup(i1, TMmul(i1, r0)))))),
TMtup(TMint(0), TMint(1))))
end // end of [TMfact2]
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_test.dats] *)
