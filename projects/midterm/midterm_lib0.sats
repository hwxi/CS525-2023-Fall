(* ****** ****** *)
(*
CS525-2023-Fall: midterm
*)
(* ****** ****** *)
#staload "./midterm.sats"
(* ****** ****** *)
//
val TMint_forall: term
val TMstr_forall: term
val TMlist_forall: term
//
(* ****** ****** *)

val TMstr_make_fwork: term
val TMlist_make_fwork: term

(* ****** ****** *)
//
val TMlistize: term
val TMrlistize: term
//
val TMstreamize: term
//
(* ****** ****** *)
//
val TMforall_get_at: term
val TMforall_foreach_at: term
//
(* ****** ****** *)
//
val TMforeach_map_list: term
//
(* ****** ****** *)
//
val TMforeach_foldleft: term
val TMforeach_foldright: term
//
(* ****** ****** *)
//
(*
HX-2023-10-21:
Please implement the following
thress programs in LAMBDA
*)
//
val TMword_buddy_stream: term
val TMlist_permute_stream: term
val TMnqueen_puzzle_solve: term
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/midterm_lib0.sats] *)
