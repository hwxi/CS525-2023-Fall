(* ****** ****** *)
#use "./../../assign3.ml";;
(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)

let list_map =
fun xs ->
foreach_to_map_list(list_foreach)(xs)

(* ****** ****** *)

let
string_fset_at
(cs: string)(i0: int)(c0: char) =
string_tabulate
(string_length(cs))
(
fun i ->
if i <> i0 then string_get_at(cs)(i) else c0)
;;
(* ****** ****** *)

let
alphabet =
string_tabulate(26)(fun i -> chr(ord('a') + i));;

(* ****** ****** *)

let
list_of_buddies
(word: string): string list =
let n0 =
string_length(word) in
list_make_fwork
(
fun work ->
int1_foreach(n0)
(
fun i0 ->
let c0 =
string_get_at(word)(i0) in
string_foreach(alphabet)
(fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
;; 
(* ****** ****** *)

let test1 = list_of_buddies("love");;
let test2 = list_of_buddies("live");;

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign3-4.ml] *)
