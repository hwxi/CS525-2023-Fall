(*
//
HX-2023-02-16: 30 points
//
Here is an implementation of the famous 8-queen puzzle:
https://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x631.html
//
Please give a NON-RECURSIVE implementation that solves the 8-queen puzzle.
//
type board_t =
int * int * int * int * int * int * int * int
//
fun
queen8_puzzle_solve(): board_t list =
(*
returns a list of boards consisting of all the solutions to the puzzle.
*)
//
*)

(* ****** ****** *)
val N = 8
(* ****** ****** *)

type
int8 =
int*int*int*int
*
int*int*int*int
type board_t = int8

(* ****** ****** *)

val
board_foreach =
fn
( bd: board_t
, work: int -> unit) =>
let
val
(x0, x1, x2, x3, x4, x5, x6, x7) = bd
in
work(x0); work(x1);
work(x2); work(x3);
work(x4); work(x5); work(x6); work(x7)
end

(* ****** ****** *)

val
board_get_at =
fn(bd: board_t, i: int) =>
foreach_to_get_at(board_foreach)(bd, i)
val
board_tabulate =
fn(f: int -> int) =>
(f(0), f(1), f(2), f(3), f(4), f(5), f(6), f(7))
val
board_fset_at =
fn(bd: board_t, i: int, p: int) =>
board_tabulate
(fn(j) => if i = j then p else board_get_at(bd, j))

(* ****** ****** *)

val
board_safety =
fn(bd, i) =>
let
val pi =
  board_get_at(bd, i)
val
helper =
fn(j) =>
let
  val pj =
  board_get_at(bd, j)
in
  pi <> pj andalso
  (abs(i-j) <> abs(pi-pj))
end
in
  int1_forall(i, fn j => helper(j))
end

(* ****** ****** *)
(*
val
int1_map_list =
fn(xs, fopr) =>
foreach_to_map_list(int1_foreach)(xs, fopr)
*)
(* ****** ****** *)
val
board_extend =
fn(bd: board_t, i: int) =>
list_filter
(int1_map_list
 (N, fn(p) => board_fset_at(bd, i, p+1)), fn(bd) => board_safety(bd, i))
(* ****** ****** *)
val
queen8_puzzle_solve =
fn() =>
int1_foldleft
( N, [board_tabulate(fn _ => 0)]
, fn(bds, i) => list_concat(list_map(bds, fn(bd) => board_extend(bd, i))))
(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/nqueen_puzzle_solve.sml] *)