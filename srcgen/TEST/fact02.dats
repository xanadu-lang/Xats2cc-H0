(* ****** ****** *)
#staload
"prelude/DATS/gnum.dats"
(* ****** ****** *)
#staload
"prelude/DATS/gord.dats"
(* ****** ****** *)
#staload
"prelude/DATS/gint.dats"
#staload
"prelude/DATS/gflt.dats"
(* ****** ****** *)
#staload
"prelude/DATS/CATS/CC/basics.dats"
(* ****** ****** *)

typedef dbl = double

(* ****** ****** *)

fun
fact(n: dbl): dbl =
if n > 0 then n * fact(n-1) else 1.0

(* ****** ****** *)

(* end of [fact02.dats] *)
