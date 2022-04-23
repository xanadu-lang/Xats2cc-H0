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
// Start Time: February, 2022
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
(* ****** ****** *)
#symload
compare with
$STM.cmp_stamp_stamp
#symload
compare with
$SYM.cmp_symbol_symbol
(* ****** ****** *)
#symload
fprint with $STM.fprint_stamp
(* ****** ****** *)

fun
htcst_compare
( htc1: htcst
, htc2: htcst): int =
compare
(htc1.stamp(), htc2.stamp())
fun
htvar_compare
( htv1: htvar
, htv2: htvar): int =
compare
(htv1.stamp(), htv2.stamp())
(* ****** ****** *)
#symload compare with htcst_compare
#symload compare with htvar_compare
(* ****** ****** *)

local

(* ****** ****** *)
#define H0Tbas_t 10
#define H0Tcst_t 20
#define H0Tvar_t 30
#define H0Tlft_t 40
#define H0Tfun_t 50
#define H0Tapp_t 60
#define H0Tlam_t 70
#define H0Ttyext_t 80
#define H0Ttyrec_t 90
#define H0Tnone1_t 100
(* ****** ****** *)

in(* in-of-local *)

fun
h0typ_tag
(h0t0: h0typ): int =
(
case+
h0t0.node() of
| H0Tbas _ => H0Tbas_t
//
| H0Tcst _ => H0Tcst_t
| H0Tvar _ => H0Tvar_t
//
| H0Tlft _ => H0Tlft_t
//
| H0Tfun _ => H0Tfun_t
//
| H0Tapp _ => H0Tapp_t
| H0Tlam _ => H0Tlam_t
//
| H0Ttyext _ => H0Ttyext_t
//
| H0Ttyrec _ => H0Ttyrec_t
//
| H0Tnone1 _ => H0Tnone1_t
) (* end of [ht0yp_tag] *)

(* ****** ****** *)

#symload .tag with h0typ_tag

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

fun
auxteq
( h0t1: h0typ
, h0t2: h0typ): int =
(
case+
h0t1.node() of
|
H0Tbas(sym1) =>
(
case-
h0t2.node() of
|
H0Tbas(sym2) => compare(sym1, sym2)
)
//
|
H0Tcst(htc1) =>
(
case-
h0t2.node() of
|
H0Tcst(htc2) => compare(htc1, htc2)
)
//
|
H0Tvar(htv1) =>
(
case-
h0t2.node() of
|
H0Tvar(htv2) => compare(htv1, htv2)
)
//
|
H0Tlft(h0t1) =>
(
case-
h0t2.node() of
|
H0Tlft(h0t2) => compare(h0t1, h0t2)
)
//
|
H0Tfun
(npf1, hts1, h0t1) =>
(
case-
h0t2.node() of
|
H0Tfun
(npf2, hts2, h0t2) =>
let
val sgn = compare(h0t1, h0t2)
in
  if (sgn != 0)
  then sgn else auxlst( hts1, hts2 )
end
)
//
|
H0Tapp
(h0t1, hts1) =>
(
case-
h0t2.node() of
|
H0Tapp
(h0t2, hts2) =>
let
val sgn = compare(h0t1, h0t2)
in
  if (sgn != 0)
  then sgn else auxlst(hts1, hts2)
end
)
//
|
H0Tlam
(svs1, h0t1) =>
(
case-
h0t2.node() of
|
H0Tlam
(svs2, h0t2) => compare(h0t1, h0t2)
)
//
|
H0Ttyext
(nam1, hts1) =>
(
case-
h0t2.node() of
|
H0Ttyext
(nam2, hts2) =>
let
val sgn = compare(nam1, nam2)
in
  if (sgn != 0)
  then sgn else auxlst( hts1, hts2 )
end
)
//
|
H0Ttyrec
(knd1, npf1, lhts1) =>
(
case-
h0t2.node() of
|
H0Ttyrec
(knd2, npf2, lhts2) => (~1)
)
//
|
H0Tnone1 _ =>
(case- h0t2.node() of H0Tnone1 _ => 0)
//
) (*case*) // end of [auxteq]

(* ****** ****** *)

and
auxlst
( xs: h0typlst
, ys: h0typlst): int =
(
case+ xs of
//
|
list_nil() =>
(
case+ ys of
|
list_nil() => (0)
|
list_cons _ => (~1)
)
//
|
list_cons(x1, xs) =>
(
case+ ys of
|
list_nil() => (1)
|
list_cons(y1, ys) =>
let
val sgn = compare(x1, y1)
in
if (sgn != 0) then sgn else auxlst(xs, ys)
end // end of [list_cons]
)
//
) (*case*) // end of [auxlst]

(* ****** ****** *)

in(*in-of-local*)

implement
h0typ_compare
(h0t1, h0t2) =
let
val tag1 = h0t1.tag()
val tag2 = h0t2.tag()
val cmp3 = compare(tag1, tag2)
in
if
(cmp3 = 0)
then auxteq(h0t1, h0t2) else cmp3
end

end // end of [local]

(* ****** ****** *)

(* end of [xats_interp1_util0.dats] *)
