(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Xanadu - Unleashing the Potential of Types!
** Copyright (C) 2022 Hongwei Xi, ATS Trustful Software, Inc.
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
// Start Time: January, 2022
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
#staload $INTREP0(* open *)
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
#staload "./../SATS/tcomp01.sats"
(* ****** ****** *)
extern
fun
tcomp01_h0pat(h0pat): void
extern
fun
tcomp01_h0patlst(h0patlst): void
(* ****** ****** *)
extern
fun
tcomp01_hfarg(hfarg): void
extern
fun
tcomp01_hfarglst(hfarglst): void
(* ****** ****** *)
extern
fun
tcomp01_h0exp(h0exp): void
extern
fun
tcomp01_h0explst(h0explst): void
extern
fun
tcomp01_h0expopt(h0expopt): void
(* ****** ****** *)
extern
fun
tcomp01_h0gpat(h0gpat): void
extern
fun
tcomp01_h0gualst(h0gualst): void
(* ****** ****** *)
extern
fun
tcomp01_h0clau(h0clau): void
extern
fun
tcomp01_h0claulst(h0claulst): void
(* ****** ****** *)
//
implement
fprint_val<h0typ> = fprint_h0typ
//
(* ****** ****** *)
//
implement
fprint_val<h0pat> = fprint_h0pat
implement
fprint_val<hfarg> = fprint_hfarg
//
implement
fprint_val<h0exp> = fprint_h0exp
//
(* ****** ****** *)
implement
fprint_val<htqarg> = fprint_htqarg
implement
fprint_val<htiarg> = fprint_htiarg
(* ****** ****** *)

implement
tcomp01_h0patlst
  ( h0ps ) =
(
case+ h0ps of
|
list_nil() => ()
|
list_cons(h0p1, h0ps) =>
{
val () = tcomp01_h0pat(h0p1)
val () = tcomp01_h0patlst(h0ps)
}
) // end of [tcomp01_h0patlst]

(* ****** ****** *)

implement
tcomp01_h0explst
  ( h0es ) =
(
case+ h0es of
|
list_nil() => ()
|
list_cons(h0e1, h0es) =>
{
val () = tcomp01_h0exp(h0e1)
val () = tcomp01_h0explst(h0es)
}
) // end of [tcomp01_h0explst]

(* ****** ****** *)

implement
tcomp01_h0dclist
  ( dcls ) =
(
case+ dcls of
|
list_nil() => ()
|
list_cons(dcl1, dcls) =>
{
val () = tcomp01_h0dcl(dcl1)
val () = tcomp01_h0dclist(dcls)
}
)(*case*)//end of [tcomp01_h0dclist]

(* ****** ****** *)

(* end of [xats_tcomp01_dynexp.dats] *)
