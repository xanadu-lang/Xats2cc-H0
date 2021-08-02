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
"./../../xatsopt/srcgen/xats"
(* ****** ****** *)
//
#staload
"{$XATSOPT}/SATS/lexing0.sats"
//
#staload
"{$XATSOPT}/SATS/intrep0.sats"
//
(* ****** ****** *)

absvtype
compenv_vtbox = ptr
vtypedef
compenv = compenv_vtbox

(* ****** ****** *)
fun
compenv_make_nil
  ((*void*)): compenv
(* ****** ****** *)
//
fun
xcomp01_package
(h0pkg: h0comped): l1pkg
//
(* ****** ****** *)
fun
xcomp01_flevget
(env0: !compenv): int
fun
xcomp01_flevinc
(env0: !compenv): void
fun
xcomp01_flevdec
(env0: !compenv): void
(* ****** ****** *)

(* end of [xats_xcomp01.sats] *)