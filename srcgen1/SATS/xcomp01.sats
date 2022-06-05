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
fun
compenv_free_top
  ( compenv ): l1tmplst
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
fun
xcomp01_dvarfind
( env0:
! compenv
, hdv0
: h0var): l1valopt_vt
(* ****** ****** *)
//
fun
xcomp01_l1valize
( env0:
! compenv
, l1v0: l1val): l1val
//
(* ****** ****** *)
//
fun
xcomp01_dvaradd_bind
( env0:
! compenv
, hdv0: h0var
, l1v1: l1val): void
//
fun
xcomp01_dvaradd_fun0
  (env0: !compenv): void
fun
xcomp01_dvarpop_fun0
  (env0: !compenv): void
//
(* ****** ****** *)
//
(*
fun
xcomp01_ltmpadd_ltmp
( env0:
! compenv
, ltmp: l1tmp): void
*)
//
fun
xcomp01_ltmpadd_fun0
( env0: !compenv ): void
fun
xcomp01_ltmppop_fun0
( env0: !compenv ): l1tmplst
//
fun
xcomp01_ltmpnew_tmp0
( env0:
! compenv, loc_t ): l1tmp
fun
xcomp01_ltmpnew_arg1
( env0:
! compenv, loc_t, int): l1tmp
//
(* ****** ****** *)
//
fun
xcomp01_lcmdadd_lcmd
( env0:
! compenv
, lcmd: l1cmd): void
//
(* ****** ****** *)
//
fun
xcomp01_lcmdpush_nil
( env0: !compenv ): void
//
fun
xcomp01_lcmdpop0_blk
( env0: !compenv): l1blk
fun
xcomp01_lcmdpop0_lst
( env0: !compenv): l1cmdlst
//
(* ****** ****** *)
//
(*
fun
xcomp01_l1srt
( env0:
! compenv
, h0st: h0srt): l1srt
//
fun
xcomp01_h0typ
( env0:
! compenv, h0t0: h0typ): l1tnm
//
*)
//
(* ****** ****** *)
//
fun
xcomp01_hdcon
( env0:
! compenv, hdc0: h0con): l1con
//
(* ****** ****** *)
//
// HX:
// for checking whether
// the pattern matches the value
//
fun
xcomp01_h0pat_ck0
( env0:
! compenv
, h0p0: h0pat, l1v1: l1val): l1pck
//
// HX:
// for extracting values
// under the assumption that
// the pattern matches the value
//
fun
xcomp01_h0pat_ck1
( env0:
! compenv
, h0p0: h0pat, l1v1: l1val) : void
//
(* ****** ****** *)
//
fun
xcomp01_h0pat_ck01
( env0:
! compenv
, h0p0: h0pat, l1v1: l1val): void
//
(* ****** ****** *)

//
fun
xcomp01_h0gpat_ck0
( env0:
! compenv
, hgp0: h0gpat, l1v1: l1val): l1pck
//
fun
xcomp01_h0gpat_ck1
( env0:
! compenv
, hgp0: h0gpat, l1v1: l1val) : void
//
(* ****** ****** *)
//
typedef
l1fag = l1tmplst
typedef
l1faglst = List0(l1fag)
//
fun
xcomp01_h0faglst
( env0:
! compenv
, hfgs: h0faglst): l1faglst
fun
xcomp01_h0faglst_ck01
( env0:
! compenv
, hfgs: h0faglst, lfgs: l1faglst): l1blk
//
(* ****** ****** *)
fun
xcomp01_h0exp_val
( env0:
! compenv, h0e0: h0exp): l1val
fun
xcomp01_h0exp_set
( env0:
! compenv
, h0e0: h0exp, tres: l1tmp): void
(* ****** ****** *)
//
fun
xcomp01_h0explst_val
( env0:
! compenv
, h0es: h0explst): l1valist
//
fun
xcomp01_h0explst_arg
( env0:
! compenv
, npf: int
, h0es: h0explst(*arg*)): l1valist
//
(* ****** ****** *)
//
fun
xcomp01_h0exp_blk
( env0:
! compenv
, h0e0: h0exp, tres: l1tmp): l1blk
fun
xcomp01_h0expopt_blk
( env0:
! compenv
, opt0: h0expopt, tres: l1tmp): l1blk
//
(* ****** ****** *)
//
fun
xcomp01_h0dcl_dcl
( env0:
! compenv, dcl0: h0dcl): l1dcl
fun
xcomp01_h0dclist_dcl
( env0:
! compenv, dcls: h0dclist): l1dclist
//
(* ****** ****** *)
fun
xcomp01_h0dcl_timp
( env0:
! compenv
, l1c1: l1cst, dcl2: h0dcl): l1dcl
(* ****** ****** *)
//
fun
xcomp01_h0fundecl
( env0:
! compenv, dcl0: h0fundecl): l1fundecl
fun
xcomp01_h0fundeclist
( env0:
! compenv, dcls: h0fundeclist): l1fundeclist
//
(* ****** ****** *)
//
fun
xcomp01_h0valdecl
( env0:
! compenv, dcl0: h0valdecl): l1valdecl
fun
xcomp01_h0valdeclist
( env0:
! compenv, dcls: h0valdeclist): l1valdeclist
//
(* ****** ****** *)
//
fun
xcomp01_h0vardecl
( env0:
! compenv, dcl0: h0vardecl): l1vardecl
fun
xcomp01_h0vardeclist
( env0:
! compenv, dcls: h0vardeclist): l1vardeclist
//
(* ****** ****** *)

(* end of [xats_xcomp01.sats] *)
