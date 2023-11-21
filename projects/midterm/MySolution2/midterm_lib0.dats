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
TMord
(t1: term): term =
TMopr("ord", mylist_sing(t1))
(* ****** ****** *)
(* ****** ****** *)
fun
TMprchr
(t1: term): term =
TMopr("prchr", mylist_sing(t1))
fun
TMprint
(t1: term): term =
TMopr("print", mylist_sing(t1))
fun
TMprstr
(t1: term): term =
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
(cs: term
,i0: term): term =
TMopr
("str_get_at", mylist_pair(cs, i0))
fun
TMstr_make_list
(cs: term): term =
TMopr("str_make_list", mylist_sing(cs))
(* ****** ****** *)
fun
TMref_new
(v0: term): term =
TMopr("ref_new", mylist_sing(v0))
fun
TMref_get
(r0: term): term =
TMopr("ref_get", mylist_sing(r0))
fun
TMref_set
(r0: term
,v0: term): term =
TMopr("ref_set", mylist_pair(r0, v0))
(* ****** ****** *)
//
fun
TMlist_nil
((*void*)): term =
TMopr("list_nil", mylist_nil())
fun
TMlist_cons
(x1: term
,xs: term): term =
TMopr("list_cons", mylist_pair(x1, xs))
//
fun
TMlist_nilq
(xs: term): term =
TMopr("list_nilq", mylist_sing(xs))
fun
TMlist_consq
(xs: term): term =
TMopr("list_consq", mylist_sing(xs))
//
fun
TMlist_uncons1
(xs: term): term =
TMopr("list_uncons1", mylist_sing(xs))
fun
TMlist_uncons2
(xs: term): term =
TMopr("list_uncons2", mylist_sing(xs))
//
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
//
fun
TMllist_nil
((*void*)): term =
TMopr("llist_nil", mylist_nil())
fun
TMllist_cons
(x1: term
,xs: term): term =
TMopr("llist_cons", mylist_pair(x1, xs))
//
fun
TMllist_nilq
(xs: term): term =
TMopr("llist_nilq", mylist_sing(xs))
fun
TMllist_consq
(xs: term): term =
TMopr("llist_consq", mylist_sing(xs))
//
fun
TMllist_uncons1
(xs: term): term =
TMopr("llist_uncons1", mylist_sing(xs))
fun
TMllist_uncons2
(xs: term): term =
TMopr("llist_uncons2", mylist_sing(xs))
//
(* ****** ****** *)
fun
TMlazy
(t1: term): term = TMlam("", t1)(*thunk*)
fun
TMeval
(t1: term): term = TMapp(t1(*thunk*), TMnil)
(* ****** ****** *)
fun
TMignr // ignored
(t1: term): term = TMlet("", t1, TMnil(*con*))
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
  TMfix("loop", "i0",
  TMif0(TMlt(i0, n0),
  TMif0(TMapp(test, i0), TMapp(loop, TMsuc(i0)), TMfalse), TMtrue)), TMint(0))))
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
//
val
TMint_foreach = TMapp(TMforall_foreach, TMint_forall)
val
TMstr_foreach = TMapp(TMforall_foreach, TMstr_forall)
//
(* ****** ****** *)
//
val
TMlist_forall =
let
val xs = TMvar"xs"
val x1 = TMvar"x1"
val test = TMvar"test"
val loop = TMvar"loop"
in//let
TMlam("xs",
TMlam("test",
  TMapp(
  TMfix("loop", "xs",
  TMif0(TMlist_nilq(xs), TMtrue,
  TMif0(TMapp(test, TMlist_uncons1(xs)), TMapp(loop, TMlist_uncons2(xs)), TMfalse))), xs)))
end // end-of-let // end of [TMlist_forall]
//
(* ****** ****** *)
val
TMlist_foreach = TMapp(TMforall_foreach, TMlist_forall)
(* ****** ****** *)
//
val
TMforeach_foldleft =
let
val x0 = TMvar"x0"
val xs = TMvar"xs"
val res = TMvar"res"
val fopr = TMvar"fopr"
val foreach = TMvar"foreach" in
TMlam("foreach",
TMlam("xs", TMlam("res", TMlam("fopr",
  TMlet("res", TMref_new(res),
  TMseq(
  TMapp(
  TMapp(foreach, xs),
  TMlam("x0", TMref_set(res, TMapp(TMapp(fopr, TMref_get(res)), x0)))), TMref_get(res)))))))
end // end-of-let // end of [TMforeach_foldleft]
//
(* ****** ****** *)
//
val
TMint_foldleft =
TMapp(TMforeach_foldleft, TMint_foreach)
val
TMstr_foldleft =
TMapp(TMforeach_foldleft, TMstr_foreach)
//
val
TMlist_foldleft =
TMapp(TMforeach_foldleft, TMlist_foreach)
//
(* ****** ****** *)
val
TMlist_length =
let
val x0 = TMvar"x0"
val xs = TMvar"xs" in
TMlam("xs",
  TMapp(
  TMapp(
  TMapp(TMlist_foldleft, xs), TMint(0)), TMlam("xs", TMlam("x0", TMsuc(xs)))))
end // end-of-let // end of [TMlist_length(...)]
(* ****** ****** *)
//
val
TMlist_rappend =
let
val x0 = TMvar"x0"
val xs = TMvar"xs"
val ys = TMvar"ys" in
TMlam("xs", TMlam("ys",
  TMapp(
  TMapp(
  TMapp(TMlist_foldleft, xs), ys), TMlam("xs", TMlam("x0", TMlist_cons(x0, xs))))))
end // end-of-let // end of [TMlist_rappend(...)]
//
val
TMlist_reverse =
let
val x0 = TMvar"x0"
val xs = TMvar"xs" in
TMlam("xs",
  TMapp(TMapp(TMlist_rappend, xs), TMlist_nil()))
end // end-of-let // end of [TMlist_reverse(...)]
//
val
TMlist_append2 =
let
val x0 = TMvar"x0"
val xs = TMvar"xs"
val ys = TMvar"ys" in
TMlam("xs", TMlam("ys",
  TMlet("xs", TMapp(TMlist_reverse, xs),
  TMapp(
  TMapp(
  TMapp(TMlist_foldleft, xs), ys), TMlam("xs", TMlam("x0", TMlist_cons(x0, xs)))))))
end // end-of-let // end of [TMlist_append2(...)]
//
(* ****** ****** *)

val
TMlist_make_fwork =
let
val x0 = TMvar"x0"
val res = TMvar"res"
val fwork = TMvar"fwork" in
TMlam("fwork",
TMlet("res", TMref_new(TMlist_nil()),
TMseq(TMapp(fwork,
TMlam("x0",
TMref_set(res, TMlist_cons(x0, TMref_get(res))))), TMapp(TMlist_reverse, TMref_get(res)))))
end // end-of-let // end of [TMlist_make_fwork(...)]

(* ****** ****** *)

val
TMstr_make_fwork =
let
val fwork = TMvar"fwork" in
TMlam("fwork", TMstr_make_list(TMapp(TMlist_make_fwork, fwork)))
end // end-of-let // end of [TMstr_make_fwork(...)]

val
TMstr_append2 =
let
val cs = TMvar"cs"
val ds = TMvar"ds"
val work = TMvar"work" in
TMlam("cs", TMlam("ds",
TMapp(
TMstr_make_fwork,
TMlam("work", TMseq(TMapp(TMapp(TMstr_foreach, cs), work), TMapp(TMapp(TMstr_foreach, ds), work))))))
end // end-of-let // end of [TMstr_append2(...)]

(* ****** ****** *)
//
val
TMfoldleft_map_list =
let
val x0 = TMvar"x0"
val xs = TMvar"xs"
val res = TMvar"res"
val fopr = TMvar"fopr"
val foldleft = TMvar"foldleft" in
TMlam("foldleft",
TMlam("xs", TMlam("fopr",
  TMapp(
  TMlist_reverse,
  TMapp(
  TMapp(
  TMapp(foldleft, xs), TMlist_nil()), TMlam("res", TMlam("x0", TMlist_cons(TMapp(fopr, x0), res))))))))
end // end-of-let // end of [TMfoldleft_map_list]
//
(* ****** ****** *)
//
val
TMfoldleft_map_rlist =
let
val x0 = TMvar"x0"
val xs = TMvar"xs"
val res = TMvar"res"
val fopr = TMvar"fopr"
val foldleft = TMvar"foldleft" in
TMlam("foldleft",
TMlam("xs", TMlam("fopr",
  TMapp(
  TMapp(
  TMapp(foldleft, xs), TMlist_nil()), TMlam("res", TMlam("x0", TMlist_cons(TMapp(fopr, x0), res)))))))
end // end-of-let // end of [TMfoldleft_map_rlist]
//
(* ****** ****** *)
//
val
TMint_map_list =
TMapp(TMfoldleft_map_list, TMint_foldleft)
val
TMstr_map_list =
TMapp(TMfoldleft_map_list, TMstr_foldleft)
//
val
TMlist_map_list =
TMapp(TMfoldleft_map_list, TMlist_foldleft)
//
(* ****** ****** *)
val
TMint_map_rlist =
TMapp(TMfoldleft_map_rlist, TMint_foldleft)
val
TMstr_map_rlist =
TMapp(TMfoldleft_map_rlist, TMstr_foldleft)
//
(* ****** ****** *)
val
TMint_listize =
let
val x0 = TMvar"x0"
val xs = TMvar"xs" in
TMlam("xs",
TMapp(TMapp(TMint_map_list, xs), TMlam("x0", x0))) end
(* ****** ****** *)
val
TMstr_listize =
let
val x0 = TMvar"x0"
val xs = TMvar"xs" in
TMlam("xs",
TMapp(TMapp(TMstr_map_list, xs), TMlam("x0", x0))) end
(* ****** ****** *)
val
TMstr_tabulate =
let
val n0 = TMvar"n0"
val fopr = TMvar"fopr" in
TMlam("n0", TMlam("fopr",
TMstr_make_list(TMapp(TMapp(TMint_map_list, n0), fopr))))
end//let//end-of-[TMstr_tabulate]
(* ****** ****** *)
val
TMstr_fset_at =
let
val c0 = TMvar"c0"
val cs = TMvar"cs"
val i0 = TMvar"i0"
val i1 = TMvar"i1" in
TMlam("cs",
TMlam("i0", TMlam("c0",
TMapp(
TMapp(
TMstr_tabulate, TMstr_len(cs)),
TMlam("i1", TMif0(TMeq(i0, i1), c0, TMstr_get_at(cs, i1)))))))
end//let//end-of-[TMstr_fset_at]
(* ****** ****** *)
//
(*
fun
mylist_of_buddies
(word: string): mylist(string) =
let
val n0 =
str_length(word)
in//let
mylist_make_fwork
(
lam work =>
int_foreach
(
n0,
lam i0 =>
let
val c0 = str_get_at(word, i0)
in//let
str_foreach
(
AB,
lam(c1) =>
if (c1 != c0) then work(str_fset_at(word, i0, c1)))
end//let//end-of-lam
) (* end-of-[int_foreach] *)
) (* end-of-[mylist_make_fwork] *)
end//let//end-of-[mylist_of_buddies]
*)
//
val
TMlist_of_buddies =
let
val n0 = TMvar"n0"
val i0 = TMvar"i0"
val c0 = TMvar"c0"
val c1 = TMvar"c1"
val word = TMvar"word"
val work = TMvar"work"
in//let
TMlam("word",
TMlet("n0", TMstr_len(word),
TMapp(
TMlist_make_fwork, TMlam("work",
TMapp(
TMapp(
TMint_foreach, n0),
TMlam("i0",
TMlet("c0",
TMstr_get_at(word, i0),
TMapp(TMapp(TMstr_foreach,
TMstr("abcdefghijklmnopqrstuvwxyz")),
TMlam("c1",
TMif0(TMeq(TMord(c0), TMord(c1)), TMnil(),
TMapp(work, TMapp(TMapp(TMapp(TMstr_fset_at, word), i0), c1))))))))))))
end//let//end-of-[TMlist_of_buddies]
//
(* ****** ****** *)
//
val
TMstream_nil =
TMlazy(TMllist_nil())
fun
TMllist_sing
(x0: term): term =
TMllist_cons(x0, TMstream_nil)
//
fun
TMstream_sing
(x0: term): term = TMlazy(TMllist_sing(x0))
//
(* ****** ****** *)

val
TMstream_map =
let
val fxs = TMvar"fxs"
val cxs = TMvar"cxs"
val aux = TMvar"aux"
val fopr = TMvar"fopr" in
TMfix
(
"aux",
"fxs",
TMlam("fopr",
TMlazy(
TMlet("cxs", TMeval(fxs),
TMif0(TMllist_nilq(cxs), TMllist_nil(),
TMllist_cons(TMapp(fopr, TMllist_uncons1(cxs)), TMapp(TMapp(aux, TMllist_uncons2(cxs)), fopr)))))))
end//let//end-of-[TMstream_map]

(* ****** ****** *)

val
TMstream_append2 =
let
val cxs = TMvar"cxs"
val fxs = TMvar"fxs"
val fys = TMvar"fys"
val aux = TMvar"aux" in
TMfix("aux", "fxs", TMlam("fys",
TMlazy(
TMlet("cxs",
TMeval(fxs),
TMif0(TMllist_nilq(cxs), TMeval(fys),
TMllist_cons(
TMllist_uncons1(cxs), TMapp(TMapp(aux, TMllist_uncons2(cxs)), fys)))))))
end//let//end-of-[TMstream_append2]

(* ****** ****** *)

val
TMlist_takeouts =
let
val x1 = TMvar"x1"
val xs = TMvar"xs"
val xxs = TMvar"xxs"
val aux = TMvar"aux" in
TMfix("aux", "xs",
TMif0(TMlist_nilq(xs), TMlist_nil(),
TMlet("x1",
TMlist_uncons1(xs),
TMlet("xs",
TMlist_uncons2(xs),
TMlist_cons(TMtup(x1, xs),
TMapp(TMapp(TMlist_map_list,
TMapp(aux, xs)), TMlam("xxs", TMtup(TMfst(xxs), TMlist_cons(x1, TMsnd(xxs))))))))))
end//let//end-of-[TMlist_takeouts]

(* ****** ****** *)

val
TMstream_concat_list =
let
val xss = TMvar"xss"
val fxs = TMvar"fxs"
val aux = TMvar"aux" in
TMfix("aux", "xss",
TMlazy(
TMif0(TMlist_nilq(xss), TMllist_nil(),
TMlet("fxs", TMlist_uncons1(xss),
TMlet("xss", TMlist_uncons2(xss), TMeval(TMapp(TMapp(TMstream_append2, fxs), TMapp(aux, xss))))))))
end//let//end-of-[TMstream_concat_list]

(* ****** ****** *)

(*
fun
{x0:t@ype}
mylist_permute
(xs: mylist(x0)): mystream(mylist(x0)) = lam() =>
(
case+ xs of
|
mylist_nil() =>
myllist_sing(mylist_nil())
|
mylist_cons(_, _) =>
mystream_concat_list<mylist(x0)>
(
mylist_map
(mylist_takeouts(xs), lam(xxs) => mystream_map(mylist_permute(xxs.1), lam xs => mylist_cons(xxs.0, xs))))())
*)
val
TMlist_permute_stream =
let
val xs = TMvar"xs"
val xxs = TMvar"xxs"
val aux = TMvar"aux" in
TMfix("aux", "xs",
TMlazy
(
TMif0(
TMlist_nilq(xs),
TMllist_sing(TMlist_nil()),
TMeval
(
TMapp(TMstream_concat_list,
TMapp(TMapp(TMlist_map_list,
TMapp(TMlist_takeouts, xs)),
TMlam("xxs", TMapp(TMapp(TMstream_map, TMapp(aux, TMsnd(xxs))), TMlam("xs", TMlist_cons(TMfst(xxs), xs))))))))))
end//let//end-of-[TMlist_permute_stream]

(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_lib0.dats] *)
