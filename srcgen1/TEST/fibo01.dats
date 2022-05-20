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

fun
fibo(n: int): int =
if
(n >= 2)
then fibo(n-1)+fibo(n-2) else n

(* ****** ****** *)

(* end of [fibo01.dats] *)
