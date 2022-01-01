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
val N2 = N1

(* ****** ****** *)

fun f1(): int = N1
fun f2(): int = f1()

(* ****** ****** *)

fun f3(x: int) = x

(* ****** ****** *)

(* end of [test01.dats] *)
