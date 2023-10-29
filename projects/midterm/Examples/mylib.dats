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
(n0: int, test: int -> bool): bool
extern
fun
str_forall
(cs: string, test: char -> bool): bool
//
(* ****** ****** *)
//
extern
fun
{a:t@ype}
mylist_forall
(xs: mylist(a), test: a -> bool): bool
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
if test(x1) then mylist_forall(xs, test) else false
//
(* ****** ****** *)

extern
fun
{xs:t@ype}
{x0:t@ype}
forall_to_foreach
( forall
: (xs, x0 -> bool) -> bool
)
: (xs, x0 -> void) -> void

implement
{xs}
{x0}
forall_to_foreach
(forall) =
lam(xs: xs, work: x0 -> void) =>
let
val _ =
forall
(xs, lam(x0: x0) => (work(x0); true))
in
  ( (*void*) )
end (* end of [forall_to_foreach]: let *)

(* ****** ****** *)
//
extern
fun
{xs:t@ype}
{x0:t@ype}
{r0:t@ype}
foreach_to_foldleft
( foreach
: (xs, x0 -> void) -> void
)
: (xs, r0, (r0, x0) -> r0) -> r0
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
foreach_to_foldleft
(forall_to_foreach(mylist_forall))
(xs, mylist_nil, lam(r0, x0) => mylist_cons(x0, r0))

(* ****** ****** *)

extern
fun
{xs:t@ype}
{x0:t@ype}
foreach_to_listize
(
foreach:
(xs, x0->void)->void): xs -> mylist(x0)
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
(xs, x0->void)->void): xs -> mylist(x0)
implement
{xs}
{x0}
foreach_to_listize
(foreach) =
(
lam(xs) =>
(
foreach_to_foldleft(foreach)
(xs, mylist_nil, lam(r0, x0) => mylist_cons(x0, r0))))

(* ****** ****** *)

(* end of [CS525-2023-Fall/projects/midterm/examples/mylib.dats] *)
