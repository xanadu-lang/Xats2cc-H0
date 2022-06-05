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
fibo(n: int): int =
if
(n >= 2)
then fibo(n-1)+fibo(n-2) else n

(* ****** ****** *)

(* end of [fibo01.dats] *)
