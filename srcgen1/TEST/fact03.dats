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
//
fun
fact(n) = loop(n, 0, 1)
and
loop(n: int, i: int, r: int): int =
if i < n then loop(n, i+1, (i+1)*r) else r
//
(* ****** ****** *)

(* end of [fact03.dats] *)
