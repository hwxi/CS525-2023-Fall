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
implement main0() = ((*void*))
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
(* ****** ****** *)
implement
print_type(tp) =
fprint_type(stdout_ref, tp)
(* ****** ****** *)
implement
fprint_val<type> = fprint_type
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
(* ****** ****** *)
implement
print_term(t0) =
fprint_term(stdout_ref, t0)
(* ****** ****** *)
implement
fprint_val<term> = fprint_term
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
(* ****** ****** *)
implement
print_value(v0) =
fprint_value(stdout_ref, v0)
(* ****** ****** *)
implement
fprint_val<value> = fprint_value
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
VALstr(s0) =>
fprint!(out, "VALstr(", s0, ")")
//
|
VALtup(v1, v2) =>
fprint!
(out, "VALtup(", v1, ";", v2, ")")
//
|
VALlam _ =>
fprint!(out, "VALlam(", "...", ")")
|
VALfix _ =>
fprint!(out, "VALfix(", "...", ")")
//
) (*case+*) // end of [fprint_value(out, v0)]
//
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
(* ****** ****** *)

#include "./midterm_tpck.dats"
#include "./midterm_eval.dats"

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
end // end-of-let
with ~TypeError() => println!("TPomega: type error!")

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
TMadd
(i0, TMint(1)), TMapp(f, TMtup(i1, TMmul(i1, r0)))))),
TMtup(TMint(0), TMint(1))))
end // end of [TMfact2]

(* ****** ****** *)
//
val () =
try
let
val
TPfact2 =
term_type0(TMfact2)
in
//
println!("TPfact2 = ", TPfact2);
//
let
val VALfact2_10 =
term_eval0
(TMapp(TMfact2, TMint(10))) in
println!("VALfact2_10 = ", VALfact2_10) end
//
end with ~TypeError() => println!("TPfact2: type error!")
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_main.dats] *)
