(* ****** ****** *)
datatype exp =
  | EXPint of int
  | EXPneg of exp
  | EXPadd of (exp, exp)
(* ****** ****** *)
extern
fun
fprint_exp
(out: FILEref, x0: exp): void
(* ****** ****** *)

implement
fprint_exp(out, x0) =
(
case+ x0 of
| EXPint(i) =>
  (
  fprint_string(out, "EXPint(");
  fprint_int(out, i);
  fprint_string(out, ")");
  )
| EXPneg(e1) =>
  (
  fprint_string(out, "EXPneg(");
  fprint_exp(out, e1);
  fprint_string(out, ")");
  )
| EXPadd(e1, e2) =>
  (
  fprint_string(out, "EXPadd(");
  fprint_exp(out, e1);
  fprint_string(out, ";");
  fprint_exp(out, e2);
  fprint_string(out, ")");
  )
)

(* ****** ****** *)

val () =
fprint_exp
( stdout_ref
, EXPadd(EXPint(1), EXPint(2)))

(* ****** ****** *)

implement main() = 0 // HX: this is a dummy

(* ****** ****** *)

(* end of [CS525-2023-Fall/lectures/lecture-09-18.dats] *)
