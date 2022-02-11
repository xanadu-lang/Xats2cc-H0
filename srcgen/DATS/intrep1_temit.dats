(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Xanadu - Unleashing the Potential of Types!
** Copyright (C) 2021 Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi
// Start Time: August, 2021
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#include
"./../HATS/libxats2cc.hats"
//
(* ****** ****** *)
#staload $LEX(* open *)
(* ****** ****** *)
#staload $S1E(* open *)
#staload $S2E(* open *)
(* ****** ****** *)
#staload $INTREP0(* open *)
(* ****** ****** *)
overload
fprint with $STM.fprint_stamp
overload
fprint with $SYM.fprint_symbol
overload
fprint with $LOC.fprint_location
(* ****** ****** *)
overload
fprint
with $FP0.fprint_filpath_full2
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
(* ****** ****** *)

(* For emitting static code *)

(* ****** ****** *)

implement
xemit01_htcst
( out, htc ) =
(
fprint!(out, htc.sym())
) (* end of [xemit01_htcst] *)

(* ****** ****** *)

local

(* ****** ****** *)

fun
auxcst
( out
: FILEref
, l1t0: l1typ): void =
let
//
val-
L1TYPcst
( htc0 ) = l1t0.node()
//
in
  xemit01_htcst(out, htc0)
end (*let*) // end of [auxcst]

(* ****** ****** *)

fun
auxapp
( out
: FILEref
, l1t0: l1typ): void =
let
//
val-
L1TYPapp
(f0, xs) = l1t0.node()
//
val () =
xemit01_l1typ(out, f0)
val () =
xemit01_txt00(out, "(")
val () =
auxarg(out, 0(*i0*), xs)
val () =
xemit01_txt00(out, ")")
in
  // nothing
end (*let*) // end of [auxapp]

and
auxarg
( out
: FILEref
, i0: int
, xs: l1typlst): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x1, xs) =>
(
case+
x1.node() of
|
L1TYPnone0 _ =>
auxarg(out, i0, xs)
|
L1TYPnone1 _ =>
auxarg(out, i0, xs)
|
_ (* else *) =>
(
  auxarg(out, i0+1, xs)
) where
{
  val () =
  if
  (i0 > 0)
  then
  xemit01_txt00(out, ", ")
  val () =
  xemit01_l1typ( out, x1 )
}
)
) (*case*) // end of [auxarg]

(* ****** ****** *)

fun
aux_tyext
( out
: FILEref
, l1t0: l1typ): void =
let
val-
L1TYPtyext
( f0, xs )=l1t0.node()
val () =
xemit01_txt00(out, f0)
in
case+ xs of
| list_nil() => ()
| list_cons _ =>
  auxlst(out, 0(*i0*), xs)
end (*let*) // end of [aux_tyext]

and
auxlst
( out
: FILEref
, i0: int
, xs: l1typlst): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x1, xs) =>
(
case+
x1.node() of
|
_ (* else *) =>
(
  auxarg(out, i0+1, xs)
) where
{
  val () =
  if
  (i0 > 0)
  then
  xemit01_txt00(out, ", ")
  val () =
  xemit01_l1typ( out, x1 )
}
)
) (*case*) // end of [auxlst]

(* ****** ****** *)

in (* in-of-local *)

implement
xemit01_l1typ
(out, l1t0) =
(
case+
l1t0.node() of
| L1TYPcst _ =>
  auxcst(out, l1t0)
| L1TYPapp _ =>
  auxapp(out, l1t0)
| L1TYPtyext _ =>
  aux_tyext(out, l1t0)
| _(* else *) => fprint(out, l1t0)
)

end // end of [local]

(* ****** ****** *)

(* For handling datacon code *)

(* ****** ****** *)

(* end of [xats_intrep1_temit.dats] *)
