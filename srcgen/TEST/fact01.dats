(* ****** ****** *)
#staload
"prelude/DATS/gnum.dats"
(* ****** ****** *)
#staload
"prelude/DATS/gord.dats"
(* ****** ****** *)
#staload
"prelude/DATS/gint.dats"
(* ****** ****** *)
#staload
"prelude/DATS/CATS/CC/basics.dats"
(* ****** ****** *)

val N1 = 10
var N2 = 10
////
(* ****** ****** *)

fun
fact(n: int): int =
if n > 0 then n * fact(n-1) else 1

(* ****** ****** *)

(* end of [fact01.dats] *)
