(* ****** ****** *)
#staload
"prelude/DATS/gnum000.dats"
(* ****** ****** *)
#staload
"prelude/DATS/gord000.dats"
(* ****** ****** *)
#staload
"prelude/DATS/gint000.dats"
(* ****** ****** *)
#staload
"prelude\
/DATS/CATS/CC/basics0.dats"
(* ****** ****** *)

fun
fact(n: int): int =
if n > 0 then n * fact(n-1) else 1

(* ****** ****** *)

(* end of [fact01.dats] *)
