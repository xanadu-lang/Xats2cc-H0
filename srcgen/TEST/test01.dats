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
(*
#extern
fun
XATS2CC_gint_add_sint_sint
{i,j:int}
( x
: sint(i)
, y
: sint(j)): sint( i+j ) = $exname()
impltmp
gint_add_sint_sint<> = XATS2CC_gint_add_sint_sint
*)
//
(* ****** ****** *)
//
val N1 = 10
val N2 = N1
val N3 = N1 + N2
//
(* ****** ****** *)

fun f1(): int = N1
fun f2(): int = f1()

(* ****** ****** *)

fun f3(x: int) = x
fun f4(x: int) = x + x

(* ****** ****** *)

fun
fact(x: int): int =
if x > 0 then x * fact(x-1) else 1

(* ****** ****** *)

(* end of [test01.dats] *)
