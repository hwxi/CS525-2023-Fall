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
(
case+ !r0 of
|
myoptn_nil() =>
let
val p0 = ref_get_ptr(r0) in
fprint!(out, "TPxyz(", p0, ")")
end // end of [myoptn_nil]
|
myoptn_cons(T1) =>
fprint!(out, "TPxyz(", T1, ")")
)
//
|
TPref(T1) =>
fprint!(out, "TPref(", T1, ")")
//
(*
|
TPlazy(T1) =>
fprint!(out, "TPlazy(", T1, ")")
*)
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
//
val-
VALnil() =
term_eval0
(TMapp(TMapp
(TMstr_foreach, TMstr"Hello, world!\n"), TMlam("c", TMprchr(TMvar"c"))))
//
(* ****** ****** *)
//
val () =
try
let
val
TPlist_forall =
term_type0(TMlist_forall)
in//let
println!
("TPlist_forall = ", TPlist_forall)
end // end-of-let
with ~TypeError() => println!("TPlist_forall: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPlist_foreach =
term_type0(TMlist_foreach)
in//let
println!
("TPlist_foreach = ", TPlist_foreach)
end // end-of-let
with ~TypeError() => println!("TPlist_foreach: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPforeach_foldleft =
term_type0(TMforeach_foldleft)
in//let
println!
("TPforeach_foldleft = ", TPforeach_foldleft)
end // end-of-let
with ~TypeError() => println!("TPforeach_foldleft: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPlist_foldleft =
term_type0(TMlist_foldleft)
in//let
println!
("TPlist_foldleft = ", TPlist_foldleft)
end // end-of-let
with ~TypeError() => println!("TPlist_foldleft: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPlist_length =
term_type0(TMlist_length)
in//let
println!
("TPlist_length = ", TPlist_length)
end // end-of-let
with ~TypeError() => println!("TPlist_length: type error!")
//
val () =
try
let
val
TPlist_rappend =
term_type0(TMlist_rappend)
in//let
println!
("TPlist_rappend = ", TPlist_rappend)
end // end-of-let
with ~TypeError() => println!("TPlist_rappend: type error!")
//
val () =
try
let
val
TPlist_reverse =
term_type0(TMlist_reverse)
in//let
println!
("TPlist_reverse = ", TPlist_reverse)
end // end-of-let
with ~TypeError() => println!("TPlist_reverse: type error!")
//
val () =
try
let
val
TPlist_append2 =
term_type0(TMlist_append2)
in//let
println!
("TPlist_append2 = ", TPlist_append2)
end // end-of-let
with ~TypeError() => println!("TPlist_append2: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPint_map_list =
term_type0(TMint_map_list)
in//let
println!
("TPint_map_list = ", TPint_map_list)
end // end-of-let
with ~TypeError() => println!("TPint_map_list: type error!")
//
val () =
try
let
val
TPstr_map_list =
term_type0(TMstr_map_list)
in//let
println!
("TPstr_map_list = ", TPstr_map_list)
end // end-of-let
with ~TypeError() => println!("TPstr_map_list: type error!")
//
(* ****** ****** *)
//
val () =
println!
("TMstr_listize: res = ", res) where
{ val res =
  term_eval0
  (TMapp(TMstr_listize, TMstr"Hello, world!")) }
//
//
val () =
println!
("TMlist_append2: res = ", res) where
{
  val cs1 = TMapp(TMstr_listize, TMstr"Hello")
  val cs2 = TMapp(TMstr_listize, TMstr", world!")
  val res =
  term_eval0(TMapp(TMapp(TMlist_append2, cs1), cs2)) }
//
(* ****** ****** *)
//
val () =
try
let
val
TPint_map_rlist =
term_type0(TMint_map_rlist)
in//let
println!
("TPint_map_rlist = ", TPint_map_rlist)
end // end-of-let
with ~TypeError() => println!("TPint_map_rlist: type error!")
//
val () =
try
let
val
TPstr_map_rlist =
term_type0(TMstr_map_rlist)
in//let
println!
("TPstr_map_rlist = ", TPstr_map_rlist)
end // end-of-let
with ~TypeError() => println!("TPstr_map_rlist: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstr_make_fwork =
term_type0(TMstr_make_fwork)
in//let
println!
("TPstr_make_fwork = ", TPstr_make_fwork)
end // end-of-let
with ~TypeError() => println!("TPstr_make_fwork: type error!")
//
val () =
try
let
val
TPlist_make_fwork =
term_type0(TMlist_make_fwork)
in//let
println!
("TPlist_make_fwork = ", TPlist_make_fwork)
end // end-of-let
with ~TypeError() => println!("TPlist_make_fwork: type error!")
//
(* ****** ****** *)
//
val () =
println!
("TMstr_append2: res = ", res) where
{
  val cs1 = TMstr"Hello"
  val cs2 = TMstr", world!"
  val res = term_eval0(TMapp(TMapp(TMstr_append2, cs1), cs2)) }
//
(* ****** ****** *)
//
val () =
try
let
val
TPlist_takeouts =
term_type0(TMlist_takeouts)
in//let
println!
("TPlist_takeouts = ", TPlist_takeouts)
end // end-of-let
with ~TypeError() => println!("TPlist_takeouts: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstream_map =
term_type0(TMstream_map)
in//let
println!
("TPstream_map = ", TPstream_map)
end // end-of-let
with ~TypeError() => println!("TPstream_map: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstr_fset_at =
term_type0(TMstr_fset_at)
in//let
println!
("TPstr_fset_at = ", TPstr_fset_at)
end // end-of-let
with ~TypeError() => println!("TPstr_fset_at: type error!")
//
val () =
try
let
val
TPlist_of_buddies =
term_type0(TMlist_of_buddies)
in//let
println!
("TPlist_of_buddies = ", TPlist_of_buddies)
end // end-of-let
with ~TypeError() => println!("TPlist_of_buddies: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstream_append2 =
term_type0(TMstream_append2)
in//let
println!
("TPstream_append2 = ", TPstream_append2)
end // end-of-let
with ~TypeError() => println!("TPstream_append2: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPstream_concat_list =
term_type0(TMstream_concat_list)
in//let
println!
("TPstream_concat_list = ", TPstream_concat_list)
end // end-of-let
with ~TypeError() => println!("TPstream_concat_list: type error!")
//
(* ****** ****** *)
//
val () =
try
let
val
TPlist_permute_stream =
term_type0(TMlist_permute_stream)
in//let
println!
("TPlist_permute_stream = ", TPlist_permute_stream)
end // end-of-let
with ~TypeError() => println!("TPlist_permute_stream: type error!")
//
(* ****** ****** *)
//
val
VALlist_of_buddies =
term_eval0
(TMapp(TMlist_of_buddies, TMstr("love")))
val () =
println!
("VALlist_of_buddies: love = ", VALlist_of_buddies)
//
(* ****** ****** *)
//
local
val
VALlist_permute_stream5 =
term_eval0
(
TMapp(
TMlist_permute_stream,
TMapp(TMint_listize, TMint(5))))
in//local
val fxs = VALlist_permute_stream5
val-VALlam(fxs, env) = fxs
val-VALtup(xs1, fxs) = term_eval1(TMeval(fxs), env)
val () = println!("VALlist_permute_stream5: xs1 = ", xs1)
val-VALlam(fxs, env) = fxs
val-VALtup(xs2, fxs) = term_eval1(TMeval(fxs), env)
val () = println!("VALlist_permute_stream5: xs2 = ", xs2)
val-VALlam(fxs, env) = fxs
val-VALtup(xs3, fxs) = term_eval1(TMeval(fxs), env)
val () = println!("VALlist_permute_stream5: xs3 = ", xs3)
val-VALlam(fxs, env) = fxs
val-VALtup(xs4, fxs) = term_eval1(TMeval(fxs), env)
val () = println!("VALlist_permute_stream5: xs4 = ", xs4)
val-VALlam(fxs, env) = fxs
val-VALtup(xs5, fxs) = term_eval1(TMeval(fxs), env)
val () = println!("VALlist_permute_stream5: xs5 = ", xs5)
val-VALlam(fxs, env) = fxs
val-VALtup(xs6, fxs) = term_eval1(TMeval(fxs), env)
val () = println!("VALlist_permute_stream5: xs6 = ", xs6)
end//local
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_main.dats] *)
