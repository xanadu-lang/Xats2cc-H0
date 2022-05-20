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
"prelude/DATS/gflt.dats"
(* ****** ****** *)
#staload
"prelude/DATS/CATS/CC/basics.dats"
(* ****** ****** *)

typedef dbl = double

(* ****** ****** *)

fun
fibo(n: dbl): dbl =
if
(2 <= n)
then fibo(-1+n)+fibo(-2+n) else n

(* ****** ****** *)

(* end of [fibo02.dats] *)
