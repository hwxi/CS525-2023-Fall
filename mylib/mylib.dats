(* ****** ****** *)
//
(*
HX-2023-09-20:
Some library functions
for BU CAS CS 525 (2023F)
*)
//
(* ****** ****** *)
#staload UN =
"prelude/SATS/unsafe.sats"
(* ****** ****** *)

extern
fun
{a:t@ype}
ref_equal
(r1: ref(a), r2: ref(a)): bool

(* ****** ****** *)
#symload = with ref_equal(*ptreq*)
(* ****** ****** *)
//
datatype
myoptn(a:t@ype) =
| myoptn_nil of ()
| myoptn_cons of (a)
//
(* ****** ****** *)
extern
fun{a:t@ype}
myoptn_nilq(myoptn(a)): bool
extern
fun{a:t@ype}
myoptn_consq(myoptn(a)): bool
(* ****** ****** *)
//
extern
fun
{a:t@ype}
print_myoptn(myoptn(a)): void
//
extern
fun
{a:t@ype}
fprint_myoptn
(out:FILEref, xs:myoptn(a)): void
//
(* ****** ****** *)
#symload print with print_myoptn
#symload fprint with fprint_myoptn
(* ****** ****** *)
//
datatype
mylist(a:t@ype) =
| mylist_nil of ()
| mylist_cons of (a, mylist(a))
//
(* ****** ****** *)
extern
fun{a:t@ype}
mylist_nilq(mylist(a)): bool
extern
fun{a:t@ype}
mylist_consq(mylist(a)): bool
(* ****** ****** *)
//
extern
fun{a:t@ype}
mylist_sing(x1: a): mylist(a)
extern
fun{a:t@ype}
mylist_pair(x1:a, x2:a): mylist(a)
//
(* ****** ****** *)
//
extern
fun
{a:t@ype}
print_mylist(mylist(a)): void
//
extern
fun
{a:t@ype}
fprint_mylist
(out:FILEref, xs:mylist(a)): void
//
(* ****** ****** *)
#symload print with print_mylist
#symload fprint with fprint_mylist
(* ****** ****** *)
//
extern
fun
{(*tmp*)}
string_make_mylist(mylist(char)): string
//
(* ****** ****** *)
(* ****** ****** *)
//
datatype
myllist(a:t@ype) =
| myllist_nil of ()
| myllist_cons of (a, mystream(a))
//
where
mystream(a:t@ype) = () -<cloref1> myllist(a)
//
(* ****** ****** *)
//
implement
{a}
ref_equal
(r1, r2) =
( ref_get_ptr(r1)
= ref_get_ptr(r2) )
//
(* ****** ****** *)
(* ****** ****** *)
//
implement
{a}
myoptn_nilq(xs) =
(
case+ xs of
|
myoptn_nil _ => true| _ => false)

implement
{a}//tmp
myoptn_consq(xs) =
(
case+ xs of
|
myoptn_cons _ => true| _ => false)
//
(* ****** ****** *)

implement
{a}
print_myoptn(xs) = 
fprint_myoptn<a>(stdout_ref, xs)

(* ****** ****** *)
//
implement
{a}
fprint_myoptn
(out, xs) =
(
case+ xs of
|
myoptn_nil() =>
fprint!(out, "none")
|
myoptn_cons(x1) =>
(
fprint!(out, "some(");
fprint_val<a>(out, x1); fprint!(out, ")"))
)
//
(* ****** ****** *)
(* ****** ****** *)
//
implement
{a}
mylist_nilq(xs) =
(
case+ xs of
|
mylist_nil _ => true| _ => false)

implement
{a}//tmp
mylist_consq(xs) =
(
case+ xs of
|
mylist_cons _ => true| _ => false)
//
(* ****** ****** *)

implement
{a}//tmp
mylist_sing(x1) =
mylist_cons
(x1, mylist_nil())
implement
{a}//tmp
mylist_pair(x1, x2) =
mylist_cons(x1, mylist_sing<a>(x2))

(* ****** ****** *)

implement
{a}//tmp
print_mylist(xs) = 
fprint_mylist<a>(stdout_ref, xs)

(* ****** ****** *)

(*
implement
{a}
fprint_mylist(out, xs) =
(
case+ xs of
| mylist_nil() =>
(
  fprint(out, "mylist_nil()")
)
| mylist_cons(x1, xs) =>
(
  fprint!
  (out, "mylist_cons(");
  fprint_val<a>(out, x1);
  fprint!(out, ";", xs, ")") )
) (* end of [fprint_mylist(out,xs)] *)
*)

implement
{a}
fprint_mylist
(out, xs) =
let
//
fun
loop
(xs: mylist(a), i0: int): void =
(
case+ xs of
|
mylist_nil() => ()
|
mylist_cons(x1, xs) =>
(
if i0 > 0
then fprint(out, ";");
fprint_val<a>(out, x1); loop(xs, i0+1)))
//
in//let
fprint(out, "(");loop(xs, 0);fprint(out, ")")
end//let//end-of-[fprint_mylist(out, xs)]

(* ****** ****** *)
//
implement
{}(*tmp*)
string_make_mylist(cs) =
strnptr2string
(string_make_list($UN.cast{List0(charNZ)}(cs)))
//
(* ****** ****** *)

(* end of [CS525-2023-Fall/mylib/mylib.dats] *)
