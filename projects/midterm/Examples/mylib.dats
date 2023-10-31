(* ****** ****** *)
#staload
"./../../../mylib/mylib.dats"
(* ****** ****** *)
exception SubscriptExn
(* ****** ****** *)
//
extern
fun
str_length
(cs: string): int
extern
fun
str_get_at
(cs: string, i0: int): char
//
implement
str_length
(   cs   ) =
(
  g0u2i(string_length(cs)) )
//
implement
str_get_at(cs, i0) =
(
  if
  (i0 < 0)
  then $raise SubscriptExn
  else
  if (i0 >= n0)
  then $raise SubscriptExn
  else string_get_at(cs, i0)
) where
{
val i0 = g1ofg0(i0)
val cs = g1ofg0_string(cs)
val n0 = g1u2i(string1_length(cs)) }
//
(* ****** ****** *)
//
extern
fun
int_forall
(n0: int, test: int -<cloref1> bool): bool
extern
fun
str_forall
(cs: string, test: char -<cloref1> bool): bool
//
(* ****** ****** *)
//
extern
fun
{a:t@ype}
mylist_forall
(xs: mylist(a), test: a -<cloref1> bool): bool
//
(* ****** ****** *)
//
extern
fun
{a:t@ype}
mystream_forall
(xs: mystream(a), test: a -<cloref1> bool): bool
//
(* ****** ****** *)
//
implement
int_forall
(n0, test) =
(
  loop(0)) where
{
fun
loop(i0: int): bool =
if
(i0 >= n0)
then true else
(if
 test(i0)
 then loop(i0+1) else false)
}
//
implement
str_forall
(cs, test) =
string_forall(cs) where
{
implement
string_forall$pred<>(c1) = test(c1)
}
//
(* ****** ****** *)
//
implement
{a}(*tmp*)
mylist_forall
(xs, test) =
case+ xs of
|
mylist_nil() => true
|
mylist_cons(x1, xs) =>
if test(x1)
then mylist_forall(xs, test) else false
//
(* ****** ****** *)
//
implement
{a}(*tmp*)
mystream_forall
(fxs, test) =
case+ fxs() of
|
myllist_nil() => true
|
myllist_cons(x1, fxs) =>
if test(x1)
then mystream_forall(fxs, test) else false
//
(* ****** ****** *)

extern
fun
{xs:t@ype}
{x0:t@ype}
forall_to_foreach
( forall
: (xs, x0 -<cloref1> bool) -> bool
)
: (xs, x0 -<cloref1> void) -<cloref1> void

implement
{xs}
{x0}
forall_to_foreach
(forall) =
lam
( xs: xs
, work: x0 -<cloref1> void) =>
let
val _ =
forall
(xs, lam(x0: x0) => (work(x0); true))
in
  ( (*void*) )
end (* end of [forall_to_foreach]: let *)

(* ****** ****** *)
//
fun
int_foreach
( n0: int
, work: int -<cloref1> void): void =
forall_to_foreach(int_forall)(n0, work)
fun
str_foreach
( cs: string
, work: char -<cloref1> void): void =
forall_to_foreach(str_forall)(cs, work)
//
(* ****** ****** *)
//
fun
{x0:t@ype}
mylist_foreach
( xs: mylist(x0)
, work: (x0) -<cloref1> void): void =
forall_to_foreach(mylist_forall)(xs, work)
//
(* ****** ****** *)
//
extern
fun
{xs:t@ype}
{x0:t@ype}
forall_to_get_at
(
forall:
( xs
, x0 -<cloref1> bool) -> bool): (xs, int) -<cloref1> x0
//
implement
{xs}
{x0}
forall_to_get_at
(forall) =
lam(xs, i0) =>
let
val i1 = ref<int>(0)
val res =
ref<myoptn(x0)>(myoptn_nil())
in//let
if
(i0 < 0)
then raise SubscriptExn else
let
val
nfound =
forall
( xs
, lam(x0) =>
  if
  (!i1 < i0)
  then
  (!i1 := !i1 + 1; true)
  else (!res := myoptn_cons(x0); false))
in//let
if nfound
then raise SubscriptExn else
let val-myoptn_cons(x0) = !res in x0 end
end//let
end//let//end-of-[forall_to_get_at(forall)]
//
(* ****** ****** *)
//
extern
fun
{xs:t@ype}
{x0:t@ype}
{r0:t@ype}
foreach_to_foldleft
( foreach
: (xs, x0 -<cloref1> void) -> void
)
: (xs, r0, (r0, x0) -<cloref1> r0) -<cloref1> r0
//
implement
{xs}
{x0}
{r0}
foreach_to_foldleft
(foreach) =
lam(xs, r0, fopr) =>
let
val res = ref<r0>(r0)
in
foreach
( xs
, lam(x0) => !res := fopr(!res, x0)); !res
end (* end of [foreach_to_foldleft]: let *)
//
(* ****** ****** *)

extern
fun
{x0:t@ype}
mylist_reverse
(xs: mylist(x0)): mylist(x0)
implement
{x0}
mylist_reverse(xs) =
foreach_to_foldleft(mylist_foreach)
(xs, mylist_nil, lam(r0, x0) => mylist_cons(x0, r0))

(* ****** ****** *)

extern
fun
{xs:t@ype}
{x0:t@ype}
foreach_to_listize
(
foreach:
( xs
, x0-<cloref1>void)->void): xs -<cloref1> mylist(x0)
implement
{xs}
{x0}
foreach_to_listize
(foreach) =
(
lam(xs) =>
mylist_reverse
(
foreach_to_foldleft(foreach)
(xs, mylist_nil, lam(r0, x0) => mylist_cons(x0, r0))))

(* ****** ****** *)

extern
fun
{xs:t@ype}
{x0:t@ype}
foreach_to_rlistize
(
foreach:
(xs, x0-<cloref1>void)->void): xs -<cloref1> mylist(x0)
implement
{xs}
{x0}
foreach_to_rlistize
(foreach) =
(
lam(xs) =>
(
foreach_to_foldleft(foreach)
(xs, mylist_nil, lam(r0, x0) => mylist_cons(x0, r0))))

(* ****** ****** *)
//
extern
fun
{x0:t@ype}
{r0:t@ype}
mylist_foldleft
( xs
: mylist(x0)
, r0: r0, fopr: (r0, x0) -<cloref1> r0): r0
//
implement
{x0}
{r0}
mylist_foldleft
(xs, r0, fopr) =
(
  loop(xs, r0)) where
{
fun
loop
(xs: mylist(x0), r0: r0): r0 =
(
case+ xs of
|
mylist_nil() => r0
|
mylist_cons
(x1, xs) => loop(xs, fopr(r0, x1)))
}
//
(* ****** ****** *)
fun
{x0:t@ype}
mylist_length
(xs: mylist(x0)): int =
mylist_foldleft<x0><int>(xs,0,lam(r, x) => r+1)
(* ****** ****** *)
//
extern
fun
{xs:t@ype}
{x0:t@ype}
{r0:t@ype}
foreach_to_foldright
( foreach
: (xs, x0 -<cloref1> void) -> void
)
: (xs, r0, (x0, r0) -<cloref1> r0) -<cloref1> r0
//
implement
{xs}
{x0}
{r0}
foreach_to_foldright
(foreach) =
lam(xs, r0, fopr) =>
let
val xs =
mylist_reverse
(foreach_to_listize(foreach)(xs))
in//let
mylist_foldleft(xs, r0, lam(r0, x0) => fopr(x0, r0))
end//let//end-of-[foreach_to_foldright]
//
(* ****** ****** *)

extern
fun
{(*tmp*)}
str_make_fwork
( fwork
: (char -<cloref1> void) -<cloref1> void): string
extern
fun
{x0:t@ype}
mylist_make_fwork
( fwork
: ((x0) -<cloref1> void) -<cloref1> void): mylist(x0)

(* ****** ****** *)

implement
{}(*tmp*)
str_make_fwork(fwork) =
string_make_mylist(mylist_make_fwork(fwork))

(* ****** ****** *)

implement
{x0}(*tmp*)
mylist_make_fwork(fwork) =
let
val res = ref<mylist(x0)>(mylist_nil())
in//let
(
mylist_reverse(!res)) where
{
val () =
fwork(lam(x0) => (!res := mylist_cons(x0, !res)))
}
end//let//end-of-[list_make_fwork]

(* ****** ****** *)
//
extern
fun
{xs:t@ype}
{x0:t@ype}
{y0:t@ype}
foreach_to_map_list
(
foreach:
(xs, x0-<cloref1>void)->void):
(xs, x0 -<cloref1> y0) -<cloref1> mylist(y0)
//
implement
{xs}
{x0}
{y0}
foreach_to_map_list
(foreach) =
(
lam(xs, fopr) =>
mylist_reverse
(
foreach_to_foldleft(foreach)
(xs, mylist_nil, lam(r0, x0) => mylist_cons(fopr(x0), r0))))
//
(* ****** ****** *)
(* ****** ****** *)
//
fun
{x0:t@ype}
{y0:t@ype}
mylist_map
( xs
: mylist(x0)
, fopr: (x0) -<cloref1> y0): mylist(y0) =
foreach_to_map_list(mylist_foreach)(xs, fopr)
//
(* ****** ****** *)
(* ****** ****** *)
//
fun
{x0:t@ype}
mystream_nil
( (*void*) ):
mystream(x0) =
lam() => myllist_nil()
//
(* ****** ****** *)
//
fun
{x0:t@ype}
myllist_sing
( x0 : x0 ): myllist(x0) =
myllist_cons(x0, mystream_nil())
//
fun
{x0:t@ype}
mystream_sing
( x0 : x0 ): mystream(x0) =
lam() => myllist_cons(x0, mystream_nil())
//
(* ****** ****** *)
//
fun
{x0:t@ype}
{y0:t@ype}
mystream_map
( fxs
: mystream(x0)
, fopr: (x0) -<cloref1> y0): mystream(y0) =
(
  auxmain(fxs) ) where
{
fun
auxmain
( fxs
: mystream(x0)
)
: mystream(y0) = lam() =>
(
case+ fxs() of
|
myllist_nil() =>
myllist_nil()
|
myllist_cons(x1, fxs) =>
myllist_cons(fopr(x1), auxmain(fxs))) }
//
(* ****** ****** *)
//
fun
{x0:t@ype}
mystream_append
( fxs: mystream(x0)
, fys: mystream(x0)): mystream(x0) =
(
  auxmain(fxs) ) where
{
fun
auxmain(fxs: mystream(x0)): mystream(x0) = lam() =>
(
case+ fxs() of
|
myllist_nil() => fys()
|
myllist_cons(x1, fxs) => myllist_cons(x1, auxmain(fxs))) }
//
(* ****** ****** *)

(* end of [CS525-2023-Fall/projects/midterm/examples/mylib.dats] *)
