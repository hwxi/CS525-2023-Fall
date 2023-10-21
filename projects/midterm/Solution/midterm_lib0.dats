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
//
extern
val TM_int_forall: term
extern
val TM_str_forall: term
extern
val TM_list_forall: term
//
(* ****** ****** *)

extern
val TM_str_make_fwork: term
extern
val TM_list_make_fwork: term

(* ****** ****** *)
//
extern
val TM_listize: term
extern
val TM_rlistize: term
//
extern
val TM_streamize: term
//
(* ****** ****** *)
//
extern
val TM_forall_to_get_at: term
extern
val TM_forall_to_foreach_at: term
//
(* ****** ****** *)
//
extern
val TM_foreach_to_map_list: term
//
(* ****** ****** *)
//
extern
val TM_foreach_to_foldleft: term
extern
val TM_foreach_to_foldright: term
//
(* ****** ****** *)

(* end of [CS525-2022-Fall/projects/midterm/Solution/midterm_lib.dats] *)
