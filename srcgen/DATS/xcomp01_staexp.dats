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
#staload "./../SATS/xcomp01.sats"
(* ****** ****** *)

implement
xcomp01_h0typ
( env0, h0t0 ) =
(
case+
h0t0.node() of
//
|
H0Tbas(sym) =>
l1typ_make_node(L1TYPbas(sym))
//
|
H0Tcst(htc) =>
l1typ_make_node(L1TYPcst(htc))
|
H0Tvar(htv) =>
l1typ_make_node(L1TYPvar(htv))
//
|
H0Tapp(h0t1, h0ts) =>
(
  l1typ_make_node
  (L1TYPapp(l1t1, l1ts))
) where
{
  val l1t1 = 
  xcomp01_h0typ(env0, h0t1)
  val l1ts = 
  xcomp01_h0typlst(env0, h0ts)
}
//
|
H0Ttyext(name, h0ts) =>
(
  l1typ_make_node
  (L1TYPtyext(name, l1ts))
) where
{
  val l1ts =
  xcomp01_h0typlst(env0, h0ts)
}
//
|
H0Ttyrec(knd0, npf1, lhts) =>
(
  l1typ_make_node
  (L1TYPtyrec(knd0, npf1, llts))
) where
{
  val llts =
  xcomp01_labh0typlst(env0, lhts)
}
//
| _ (* else *) =>
(
  l1typ_make_node(L1TYPnone1(h0t0))
)
//
) (*case*) // end of [xcomp01_h0typ]

(* ****** ****** *)
implement
xcomp01_h0typlst
  (env0, h0ts) =
(
case+ h0ts of
|
list_nil() =>
list_nil()
|
list_cons(h0t1, h0ts) =>
let
  val l1t1 =
  xcomp01_h0typ(env0, h0t1)
in
list_cons(l1t1, l1ts) where
{
  val l1ts =
  xcomp01_h0typlst(env0, h0ts)
}
end
) (* end of [xcomp01_h0typlst] *)
(* ****** ****** *)
implement
xcomp01_labh0typlst
  (env0, lhts) =
(
case+ lhts of
|
list_nil() =>
list_nil()
|
list_cons(lht1, lhts) =>
let
val+
$S2E.SLABELED
(  l1, h0t1  ) = lht1
val l0t1 =
xcomp01_h0typ(env0, h0t1)
in
list_cons(llt1, llts) where
{
  val llt1 =
  $S2E.SLABELED(l1, l0t1)
  val llts =
  xcomp01_labh0typlst(env0, lhts)
}
end
) (* end of [xcomp01_labh0typlst] *)
(* ****** ****** *)

(* end of [xats_xcomp01_staexp.dats] *)
