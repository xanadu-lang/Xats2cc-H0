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

implement
xemit01_txt00
(out, txt) =
(
fprint(out, txt)
) (* end of [xemit01_txt00] *)
implement
xemit01_txtln
(out, txt) =
(
fprint!(out, txt, '\n')
) (* end of [xemit01_txtln] *)

(* ****** ****** *)
implement
xemit01_newln
( out ) =
(
  fprint_char(out, '\n')
) (* end of [xemit01_newln] *)
(* ****** ****** *)
//
implement
xemit01_l1tmp
(out, tmp0) =
(
  fprint_l1tmp(out, tmp0)
) (* end of [xemit01_l1tmp] *)
//
(* ****** ****** *)
//
implement
xemit01_l1dcl
(out, dcl0) =
(
  fprint_l1dcl(out, dcl0)
) (* end of [xemit01_l1dcl] *)
//
(* ****** ****** *)
//
implement
xemit01_package
  (out, lpkg) =
{
val () = auxtmps(tmps)
val () = auxdcls(dcls) 
} where
{
//
val+
L1PKG
(tmps, dcls) = lpkg
//
fun
auxtmps
( xs
: l1tmplst): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  auxtmps(xs)) where
{
val () = xemit01_l1tmp(out, x0)
val () = xemit01_txtln(out, ";")
} (* list_cons *)
) (* end of [auxtmps] *)
//
fun
auxdcls
( xs
: l1dclist): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
let
(*
val () = xindnt(0)
*)
val () =
xemit01_l1dcl(out, x0)
val () = xemit01_newln(out)
val () = xemit01_newln(out) in auxdcls(xs)
end // list_cons
) (* end of [auxdcls] *)
//
} (*where*) // end of [xemit01_package]
//
(* ****** ****** *)

(* end of [xats_intrep1_xemit.dats] *)
