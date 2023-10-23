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
(out, T0) =
(
case+ T0 of
|
TPbas(nm) =>
fprint!(out, "TPbas(", nm, ")")
//
|
TPxyz(r0) =>
fprint!(out, "TPxyz(", !r0, ")")
//
|
TPref(T1) =>
fprint!(out, "TPref(", T1, ")")
|
TPlazy(T1) =>
fprint!(out, "TPlazy(", T1, ")")
//
|
TPlist(T1) =>
fprint!(out, "TPlist(", T1, ")")
|
TPllist(T1) =>
fprint!(out, "TPllist(", T1, ")")
//
|
TPfun(T1, T2) =>
fprint!(out, "TPfun(", T1, ";", T2, ")")
|
TPtup(T1, T2) =>
fprint!(out, "TPtup(", T1, ";", T2, ")")
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
//
|
TMnil() =>
fprint!(out, "TMnil(", ")")
//
|
TMint(i0) =>
fprint!(out, "TMint(", i0, ")")
|
TMbtf(b0) =>
fprint!(out, "TMbtf(", b0, ")")
|
TMchr(c0) =>
fprint!(out, "TMchr(", c0, ")")
|
TMstr(s0) =>
fprint!(out, "TMstr(", s0, ")")
//
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
TManno(t1, T1) =>
(
  fprint!(out, "TManno(", t1, ";", T1, ")"))
//
|
TMlamt(x, Tx, tt) =>
fprint!(out, "TMlamt(", x, ";", Tx, ";", tt, ")")
|
TMfixt(f, x, Tf, tt) =>
fprint!(out, "TMfixt(", f, ";", x, ";", Tf, ";", tt, ")")
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
//
|
VALnil() =>
fprint!(out, "VALnil(", ")")
//
|
VALint(i0) =>
fprint!(out, "VALint(", i0, ")")
|
VALbtf(b0) =>
fprint!(out, "VALbtf(", b0, ")")
|
VALchr(c0) =>
fprint!(out, "VALchr(", c0, ")")
|
VALstr(s0) =>
fprint!(out, "VALstr(", s0, ")")
//
|
VALref(r0) =>
fprint!(out, "VALref(", !r0, ")")
//
|
VALtup
(v1, v2) =>
fprint!
(out, "VALtup(", v1, ";", v2, ")")
//
|
VALlst(vs) =>
(
  fprint!(out, "VALlst(", vs, ")"))
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
(* ****** ****** *)
#include "./midterm_tpck.dats"
(* ****** ****** *)
#include "./midterm_eval.dats"
(* ****** ****** *)
#include "./midterm_lib0.dats"
(* ****** ****** *)
(* ****** ****** *)
#include "./midterm_test.hats"
(* ****** ****** *)
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
println!("TPfact = ", TPfact);
let
val VALfact_10 =
term_eval0
(TMapp(TMfact, TMint(10))) in
println!("VALfact_10 = ", VALfact_10) end
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
("TPfibo = ", TPfibo);
let
val VALfibo_10 =
term_eval0
(TMapp(TMfibo, TMint(10))) in
println!("VALfibo_10 = ", VALfibo_10) end
end with ~TypeError() => println!("TPfibo: type error!")

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
//
val () =
try
let
val
TPint_forall =
term_type0(TMint_forall)
in//let
println!
("TPint_forall = ", TPint_forall)
end // end-of-let
with ~TypeError() => println!("TPint_forall: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstr_forall =
term_type0(TMstr_forall)
in//let
println!
("TPstr_forall = ", TPstr_forall)
end // end-of-let
with ~TypeError() => println!("TPstr_forall: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPforall_foreach =
term_type0(TMforall_foreach)
in//let
println!
("TPforall_foreach = ", TPforall_foreach)
end // end-of-let
with ~TypeError() => println!("TPforall_foreach: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPint_foreach =
term_type0(TMint_foreach)
in//let
println!
("TPint_foreach = ", TPint_foreach)
end // end-of-let
with ~TypeError() => println!("TPint_foreach: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstr_foreach =
term_type0(TMstr_foreach)
in//let
println!
("TPstr_foreach = ", TPstr_foreach)
end // end-of-let
with ~TypeError() => println!("TPstr_foreach: type error!")
//
(* ****** ****** *)

val-
VALnil() =
term_eval0
(
TMapp(
TMapp(
TMstr_foreach,
TMstr"Hello, world!\n"), TMlam("c", TMprchr(TMvar"c"))))

(* ****** ****** *)

val TMisprime =
let
val i0 = TMvar"i0"
val n0 = TMvar"n0" in
TMlam("n0",
TMapp(
TMapp(
TMint_forall,
TMsub(n0, TMint(2))),
TMlam("i0", TMgt(TMmod(n0, TMadd(i0, TMint(2))), TMint(0)))))
end

(* ****** ****** *)

val () =
println!("isprime = ", term_eval0(TMapp(TMisprime, TMint(727))))

(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_main.dats] *)
