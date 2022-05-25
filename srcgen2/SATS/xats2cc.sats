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
// Start Time: May 24th, 2022
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
#define
ATS_PACKNAME
"ATS3.XANADU.xats2cc-20220500"
(* ****** ****** *)
//
#define
XATSCTP_targetloc
"./../../xatsctp/srcgen"
#define
XATSOPT_targetloc
"./../../xatsopt/srcgen/xcomp"
//
(* ****** ****** *)
//
#staload LEX =
"{$XATSOPT}/SATS/lexing0.sats"
//
#staload H0E =
"{$XATSOPT}/SATS/intrep0.sats"
//
(* ****** ****** *)
//
#staload CTP =
"{$XATSCTP}/SATS/xatsctp.sats"
//
(* ****** ****** *)
abstbox l1reg_tbox = ptr
typedef l1reg = l1reg_tbox
(* ****** ****** *)
//
abstbox l1exp_tbox = ptr
typedef l1exp = l1exp_tbox
typedef l1explst = List0(l1exp)
typedef l1expopt = Option(l1exp)
//
(* ****** ****** *)
//
abstbox l1dcl_tbox = ptr
typedef l1dcl = l1dcl_tbox
typedef l1dclist = List0(l1dcl)
typedef l1dclopt = Option(l1dcl)
//
(* ****** ****** *)
typedef token = $LEX.token
typedef h0exp = $H0E.h0exp
typedef l1ctp = $CTP.l1ctp
(* ****** ****** *)
//
absvtype trccenv_vtbox
vtypedef trccenv = trccenv_vtbox
//
(* ****** ****** *)
//
datatype l1val =
//
| L1VALi00 of (int)
| L1VALb00 of (bool)
| L1VALs00 of string
//
| L1VALint of (token)
| L1VALbtf of (token)
| L1VALchr of (token)
//
| L1VALflt of (token)
| L1VALstr of (token)
//
| L1VALreg of (l1reg)
//
typedef l1valist = List0(l1val)
typedef l1valopt = Option(l1val)
//
(* ****** ****** *)
//
datatype l1cmd =
//
| L1CMDrset0 of (l1reg, l1val)
//
| L1CMDfcalx of (l1val, l1valist)
//
(* ****** ****** *)
//
fun
print_l1val: print_type(l1val)
fun
prerr_l1val: prerr_type(l1val)
fun
fprint_l1val: fprint_type(l1val)
//
#symload print with print_l1val
#symload prerr with prerr_l1val
#symload fprint with fprint_l1val
//
(* ****** ****** *)
//
fun
print_l1cmd: print_type(l1cmd)
fun
prerr_l1cmd: prerr_type(l1cmd)
fun
fprint_l1cmd: fprint_type(l1cmd)
//
#symload print with print_l1cmd
#symload prerr with prerr_l1cmd
#symload fprint with fprint_l1cmd
//
(* ****** ****** *)

(* end of [xats_xats2cc.sats] *)
