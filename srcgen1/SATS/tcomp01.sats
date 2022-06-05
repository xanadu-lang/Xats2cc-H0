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
#staload "./intrep1.sats"
(* ****** ****** *)
#define
XATSOPT_targetloc
"./../..\
/modules/xatsopt/srcgenx/."
(* ****** ****** *)
//
#staload
"{$XATSOPT}/SATS/intrep0.sats"
//
(* ****** ****** *)
fun
tcomp01_h0typ(h0typ): l1tnm
(* ****** ****** *)
//
fun
tcomp01_h0exp(h0exp) : void
fun
tcomp01_h0explst(h0explst): void
fun
tcomp01_h0expopt(h0expopt): void
//
(* ****** ****** *)
//
fun
tcomp01_h0dcl(h0dcl) : void
fun
tcomp01_h0dclist(h0dclist): void
//
(* ****** ****** *)
fun
tcomp01_the_ltnmmap((*void*)): void
(* ****** ****** *)

(* end of [xats_tcomp01.sats] *)
