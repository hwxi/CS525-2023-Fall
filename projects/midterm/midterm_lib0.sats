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
val TMstream_forall: term
//
(* ****** ****** *)
//
val TMforall_get_at: term
//
(* ****** ****** *)
//
val TMforall_foreach: term
//
(* ****** ****** *)
//
val TMforeach_listize: term
val TMforeach_rlistize: term
//
(* ****** ****** *)
//
val TMstr_make_fwork: term
val TMlist_make_fwork: term
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
three programs in LAMBDA:

TMword_buddy_list:
involving only list
TMlist_permute_stream:
involving list and llist 
TMnqueen_puzzle_solve:
involving list and llist 

*)
//
val TMword_buddy_list: term
(*
Any string is a legal word.
There is no need for a dictionary.
*)
//
val TMlist_permute_stream: term
(*
Enumerate all the permutations of
a given list.
*)
//
val TMnqueen_puzzle_solve: term
(*
You can solve N-queen or just 8-queen.
*)
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/exams/midterm/midterm_lib0.sats] *)
