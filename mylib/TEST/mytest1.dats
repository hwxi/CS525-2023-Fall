(* ****** ****** *)
#include
"share\
/atspre_staload.hats"
(* ****** ****** *)
#staload "./../mylib.dats"
(* ****** ****** *)

local
implement
fprint_val<int>
(out, x) = fprint_int(out, x)
in//local
val xs0 = mylist_nil(): mylist(int)
val xs1 = mylist_cons(1, xs0) // [1]
val xs2 = mylist_cons(2, xs1) // [2,1]
val ( ) = println!("xs2 = ", xs2)
end//local

(* ****** ****** *)

local
implement
fprint_val<string>
(out, x) = fprint_string(out, x)
in//local
val xs0 = mylist_nil(): mylist(string)
val xs1 = mylist_cons("1", xs0) // ["1"]
val xs2 = mylist_cons("2", xs1) // ["2","1"]
val ( ) = println!("xs2 = ", xs2)
end//local

(* ****** ****** *)

local
implement
fprint_val<string>
(out, x) = fprint!(out, '"', x, '"')
in//local
val xs0 = mylist_nil(): mylist(string)
val xs1 = mylist_cons("1", xs0) // ["1"]
val xs2 = mylist_cons("2", xs1) // ["2","1"]
val ( ) = println!("xs2 = ", xs2)
end//local

(* ****** ****** *)

implement main0() = ()

(* ****** ****** *)

(* end of [mytest1.dats] *)
