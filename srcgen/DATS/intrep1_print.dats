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
#staload $INTREP0(* open *)
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
(* ****** ****** *)
overload
fprint
with $SYM.fprint_symbol
(* ****** ****** *)
overload
fprint
with $LOC.fprint_location
(* ****** ****** *)
//
overload
fprint with $STM.fprint_stamp
//
overload
fprint with $LAB.fprint_label
//
overload
fprint with $LEX.fprint_token
//
(* ****** ****** *)
overload
fprint with $S2E.fprint_tyrec
(* ****** ****** *)
//
(*
implement
fprint_val<filpath> =
$FP0.fprint_filpath_full1
*)
implement
fprint_val<filpath> =
$FP0.fprint_filpath_full2
//
(* ****** ****** *)
overload
fprint with $S1E.fprint_g1exp
(* ****** ****** *)
implement
fprint_val<htcst> = fprint_htcst
(* ****** ****** *)
implement
fprint_val<h0typ> = fprint_h0typ
(* ****** ****** *)
implement
fprint_val<hdvar> = fprint_hdvar
implement
fprint_val<hdcon> = fprint_hdcon
implement
fprint_val<hfarg> = fprint_hfarg
(* ****** ****** *)
implement
fprint_val<l1tnm> = fprint_l1tnm
(* ****** ****** *)
implement
fprint_val<l1exn> = fprint_l1exn
implement
fprint_val<l1tmp> = fprint_l1tmp
(* ****** ****** *)
implement
fprint_val<l1con> = fprint_l1con
implement
fprint_val<l1cst> = fprint_l1cst
(* ****** ****** *)
implement
fprint_val<l1val> = fprint_l1val
(* ****** ****** *)
implement
fprint_val<l1pck> = fprint_l1pck
(* ****** ****** *)
implement
fprint_val<l1cmd> = fprint_l1cmd
implement
fprint_val<l1blk> = fprint_l1blk
(* ****** ****** *)

implement
fprint_val<l1dcl> = fprint_l1dcl

(* ****** ****** *)
//
implement
print_l1tnm(x0) =
fprint_l1tnm(stdout_ref, x0)
implement
prerr_l1tnm(x0) =
fprint_l1tnm(stderr_ref, x0)
//
(* ****** ****** *)
//
implement
print_l1exn(x0) =
fprint_l1exn(stdout_ref, x0)
implement
prerr_l1exn(x0) =
fprint_l1exn(stderr_ref, x0)
//
implement
fprint_l1exn(out, x0) =
fprint!
(out, "exn(", x0.stamp(), ")")
//
(* ****** ****** *)
//
implement
print_l1tmp(x0) =
fprint_l1tmp(stdout_ref, x0)
implement
prerr_l1tmp(x0) =
fprint_l1tmp(stderr_ref, x0)
//
implement
fprint_l1tmp
( out, x0 ) =
let
//
  val arg = x0.arg()
//
in(*in-of-let*)
//
if
arg <= 0
then
fprint!
(out, "tmp(", x0.stamp(), ")")
else
fprint!
( out
, "arg[", arg, "](", x0.stamp(), ")")
//
end // end of [fprint_l1tmp]
//
(* ****** ****** *)
//
implement
print_l1cst(x0) =
fprint_l1cst(stdout_ref, x0)
implement
prerr_l1cst(x0) =
fprint_l1cst(stderr_ref, x0)
implement
fprint_l1cst(out, x0) =
fprint!
(out, x0.hdc(), "(", x0.stamp(), ")")
//
(* ****** ****** *)
//
implement
print_l1con(x0) =
fprint_l1con(stdout_ref, x0)
implement
prerr_l1con(x0) =
fprint_l1con(stderr_ref, x0)
implement
fprint_l1con(out, x0) =
(
case+ x0 of
|
L1CONcon(hdc) =>
fprint!(out, "L1CONcon(", hdc, ")")
|
L1CONval(l1v) =>
fprint!(out, "L1CONval(", l1v, ")")
)
//
(* ****** ****** *)
//
implement
print_l1val(x0) =
fprint_l1val(stdout_ref, x0)
implement
prerr_l1val(x0) =
fprint_l1val(stderr_ref, x0)
//
implement
fprint_l1val(out, x0) =
(
case+
x0.node() of
//
|
L1VALi00(int) =>
fprint!(out, "L1VALi00(", int, ")")
|
L1VALb00(btf) =>
fprint!(out, "L1VALb00(", btf, ")")
|
L1VALs00(str) =>
fprint!(out, "L1VALs00(", str, ")")
//
|
L1VALint(tok) =>
fprint!(out, "L1VALint(", tok, ")")
|
L1VALbtf(tok) =>
fprint!(out, "L1VALbtf(", tok, ")")
|
L1VALchr(tok) =>
fprint!(out, "L1VALchr(", tok, ")")
//
|
L1VALflt(tok) =>
fprint!(out, "L1VALflt(", tok, ")")
|
L1VALstr(tok) =>
fprint!(out, "L1VALstr(", tok, ")")
//
|
L1VALtop(tok) =>
fprint!(out, "L1VALtop(", tok, ")")
//
|
L1VALnam(nam) =>
fprint!(out, "L1VALnam(", nam, ")")
//
(*
|
L1VALexn(exn) =>
fprint!(out, "L1VALexn(", exn, ")")
*)
|
L1VALtmp(tmp) =>
fprint!(out, "L1VALtmp(", tmp, ")")
//
|
L1VALcon(hdc) =>
fprint!(out, "L1VALcon(", hdc, ")")
//
|
L1VALcfun(hdc) =>
fprint!(out, "L1VALfcst(", hdc, ")")
//
|
L1VALctmp(l1c1, ldcl) =>
fprint!(out, "L1VAL1cst(", l1c1, ")")
//
|
L1VALctag(l1v1) =>
fprint!(out, "L1VALctag(", l1v1, ")")
|
L1VALcarg(l1v1, idx2) =>
fprint!
( out
, "L1VALcarg(", l1v1, "; ", idx2, ")")
|
L1VALcofs(l1v1, idx2) =>
fprint!
( out
, "L1VALcofs(", l1v1, "; ", idx2, ")")
//
| L1VALnone0() =>
fprint!(out, "L1VALnone0(", ")")
| L1VALnone1(h0e1) =>
fprint!(out, "L1VALnone1(", h0e1, ")")
//
| _ (* else *) => fprint!(out, "L1VAL...(...)")
//
) (* end of [fprint_l1val] *)

(* ****** ****** *)
//
implement
print_l1pck(x0) =
fprint_l1pck(stdout_ref, x0)
implement
prerr_l1pck(x0) =
fprint_l1pck(stderr_ref, x0)
//
implement
fprint_l1pck(out, x0) =
(
case+ x0 of
| L1PCKany() =>
  fprint!(out, "L1PCKany(", ")")
//
| L1PCKi00
  (int1, l1v2) =>
  fprint!
  ( out
  , "L1PCKi00(", int1, "; ", l1v2, ")")
| L1PCKb00
  (btf1, l1v2) =>
  fprint!
  ( out
  , "L1PCKb00(", btf1, "; ", l1v2, ")")
| L1PCKs00
  (str1, l1v2) =>
  fprint!
  ( out
  , "L1PCKs00(", str1, "; ", l1v2, ")")
//
| L1PCKint
  (int1, l1v2) =>
  fprint!
  ( out
  , "L1PCKint(", int1, "; ", l1v2, ")")
| L1PCKbtf
  (btf1, l1v2) =>
  fprint!
  ( out
  , "L1PCKbtf(", btf1, "; ", l1v2, ")")
| L1PCKchr
  (chr1, l1v2) =>
  fprint!
  ( out
  , "L1PCKchr(", chr1, "; ", l1v2, ")")
| L1PCKstr
  (str1, l1v2) =>
  fprint!
  ( out
  , "L1PCKstr(", str1, "; ", l1v2, ")")
//
| L1PCKcon
  (hdc1, l1v2) =>
  fprint!
  ( out
  , "L1PCKcon(", hdc1, "; ", l1v2, ")")
//
| L1PCKapp
  (pck1, pcks) =>
  fprint!
  ( out
  , "L1PCKapp(", pck1, "; ", pcks, ")")
//
| L1PCKtup
  (knd0, pcks) =>
  fprint!
  ( out
  , "L1PCKtup(", knd0, "; ", pcks, ")")
//
| L1PCKgpat
  (pck1, pcks) =>
  fprint!
  ( out
  , "L1PCKgpat(", pck1, "; ", pcks, ")")
//
| L1PCKgexp
  (l1v1, blk1) =>
  fprint!
  ( out
  , "L1PCKgexp(", l1v1, "; ", blk1, ")")
| L1PCKgmat
  (h0e1, h0p2) =>
  fprint!
  ( out
  , "L1PCKgmat(", h0e1, "; ", h0p2, ")")
//
| L1PCKxpat
  (h0p1, l1v2) =>
  fprint!
  ( out
  , "L1PCKxpat(", h0p1, "; ", l1v2, ")")
//
) (* end of [fprint_l1pck] *)
//
(* ****** ****** *)
//
implement
print_l1cmd(x0) =
fprint_l1cmd(stdout_ref, x0)
implement
prerr_l1cmd(x0) =
fprint_l1cmd(stderr_ref, x0)
//
implement
fprint_l1cmd(out, x0) =
(
case+
x0.node() of
//
|
L1CMDmov
(tres, l0v1) =>
fprint!
( out
, "L1CMDmov(", tres, "; ", l0v1, ")")
//
|
L1CMDcon
(tres, hdc1, l0vs) =>
fprint!
( out
, "L1CMDcon("
, tres, "; ", hdc1, "; ", l0vs, ")")
//
|
L1CMDtup
(tres, knd0, l0vs) =>
fprint!
( out
, "L1CMDtup("
, tres, "; ", knd0, "; ", l0vs, ")")
(*
|
L1CMDcst
(tres, hdc1, l0vs) =>
fprint!
( out
, "L1CMDcst("
, tres, "; ", hdc1, "; ", l0vs, ")")
*)
//
|
L1CMDapp
(tres, l0v1, l0vs) =>
fprint!
( out
, "L1CMDapp("
, tres, "; ", l0v1, "; ", l0vs, ")")
//
(*
|
L1CMDlam
(tres, l1am) =>
fprint!
( out,
 "L1CMDlam(", tres, "; ", l1am, ")")
|
L1CMDfix
(tres, lfix) =>
fprint!
( out,
 "L1CMDfix(", tres, "; ", lfix, ")")
*)
//
|
L1CMDlazy
(tres, l1v1) =>
fprint!
( out,
 "L1CMDlazy(", tres, "; ", l1v1, ")"
) (* L1CMDlazy *)
|
L1CMDllazy
(tres, l1v1, l1v2) =>
fprint!
( out,
 "L1CMDllazy("
 , tres, "; ", l1v1, "; ", l1v2, ")"
) (* L1CMDllazy *)
//
|
L1CMDblk(blk1) =>
fprint!(out, "L1CMDblk(", blk1, ")")
//
|
L1CMDdcl(dcl1) =>
fprint!(out, "L1CMDdcl(", dcl1, ")")
//
|
L1CMDift1
(l1v1, blk2, blk3) =>
fprint!
( out
, "L1CMDift1("
, l1v1, "; ", blk2, "; ", blk3, ")")
//
|
L1CMDcase
( knd0
, l1v1, tcas, pcks, blks) =>
fprint!
( out
, "L1CMDcase("
, knd0, "; ", l1v1, "; "
, tcas, "; ", pcks, "; ", blks, ")")
//
|
L1CMDtry0
( blk1
, texn, tcas, pcks, blks) =>
fprint!
( out
, "L1CMDtry0("
, blk1, "; ", texn, "; "
, tcas, "; ", pcks, "; ", blks, ")")
//
|
L1CMDpatck
( lpck ) =>
(
  fprint!
  (out, "L1CMDpatck(", lpck, ")")
)
|
L1CMDmatch
(h0p1, l1v2) =>
fprint!
( out
, "L1CMDmatch(", h0p1, "; ", l1v2, ")")
//
|
L1CMDflat
(tres, l1v1) =>
fprint!
( out
, "L1CMDflat(", tres, "; ", l1v1, ")")
//
|
L1CMDcarg
(tres, l1v1, idx2) =>
(
  fprint!
  ( out
  , "L1CMDcarg("
  , tres, "; ", l1v1, "; ", idx2, ")" )
)
|
L1CMDcofs
(tres, l1v1, idx2) =>
(
  fprint!
  ( out
  , "L1CMDcofs("
  , tres, "; ", l1v1, "; ", idx2, ")" )
)
//
|
L1CMDtarg
(tres, l1v1, idx2) =>
(
  fprint!
  ( out
  , "L1CMDtarg("
  , tres, "; ", l1v1, "; ", idx2, ")" )
)
|
L1CMDtofs
(tres, l1v1, idx2) =>
(
  fprint!
  ( out
  , "L1CMDtofs("
  , tres, "; ", l1v1, "; ", idx2, ")" )
)
//
| _ (* else *) => fprint!(out, "L1CMD...(...)")
//
) (* end of [fprint_l1cmd] *)
//
(* ****** ****** *)
//
implement
print_l1blk(x0) =
fprint_l1blk(stdout_ref, x0)
implement
prerr_l1blk(x0) =
fprint_l1blk(stderr_ref, x0)
//
implement
fprint_l1blk(out, x0) =
(
case+ x0 of
|
L1BLKnone() =>
fprint!(out, "L1BLKnone(", ")")
|
L1BLKsome(cmds) =>
fprint!(out, "L1BLKsome(", cmds, ")")
)
//
(* ****** ****** *)
//
implement
print_l1dcl(x0) =
fprint_l1dcl(stdout_ref, x0)
implement
prerr_l1dcl(x0) =
fprint_l1dcl(stderr_ref, x0)
//
(* ****** ****** *)

local

implement
fprint_val<lfundecl> = fprint_lfundecl
implement
fprint_val<lvaldecl> = fprint_lvaldecl
implement
fprint_val<lvardecl> = fprint_lvardecl

in(*in-of-local*)

(* ****** ****** *)

implement
fprint_l1dcl
( out, x0 ) =
(
case+
x0.node() of
//
|
L1DCLfundecl
(knd0, mopt, lfds) =>
fprint!(out, "L1DCLfundecl(", lfds, ")")
//
|
L1DCLvaldecl
(knd0, mopt, lvds) =>
fprint!(out, "L1DCLvaldecl(", lvds, ")")
//
|
L1DCLvardecl
(knd0, mopt, lvds) =>
fprint!(out, "L1DCLvardecl(", lvds, ")")
//
|
L1DCLexcptcon
  (hdcs) =>
(
fprint!(out, "L1DCLexcptcon(", hdcs, ")")
)
|
L1DCLdatatype
  (htcs) =>
(
fprint!(out, "L1DCLdatatype(", htcs, ")")
)
//
|
L1DCLimpdecl3
(knd0, mopt, limp) =>
fprint!(out, "L1DCLimpdecl3(", limp, ")")
//
|
L1DCLtimpcst3
(l1c1, dcl2(*timp*)) =>
fprint!
( out
, "L1DCLtimpcst3(", l1c1, "; ", dcl2, ")")
//
| L1DCLnone0() =>
  fprint!(out, "L1DCLnone0(", ")")
| L1DCLnone1(hdcl) =>
  fprint!(out, "L1DCLnone1(", hdcl, ")")
//
) (*where*) // end of [fprint_l1dcl]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
//
implement
print_lfundecl(x0) =
fprint_lfundecl(stdout_ref, x0)
implement
prerr_lfundecl(x0) =
fprint_lfundecl(stderr_ref, x0)
//
implement
fprint_lfundecl
  (out, x0) = let
//
val+LFUNDECL(rcd) = x0
//
in
//
case+
rcd.hag of
|
None() =>
fprint!
( out
, "LFUNDECL@{"
, "nam=", rcd.nam, "; "
, "hdc=", rcd.hdc, "; ", "}")
|
Some(rcd_hag) =>
fprint!
( out
, "LFUNDECL@{"
, "nam=", rcd.nam, "; "
, "hdc=", rcd.hdc, "; "
, "hag=", rcd_hag, "; "
, "def=", rcd.def, "; "
, "rtp=", rcd.rtp, "; "
, "lev=", rcd.lev, "; "
, "hag_blk=", rcd.hag_blk, "; "
, "def_blk=", rcd.def_blk, "; ", "}")
//
end // end of [fprint_lfundecl]
//
(* ****** ****** *)
//
implement
print_lvaldecl(x0) =
fprint_lvaldecl(stdout_ref, x0)
implement
prerr_lvaldecl(x0) =
fprint_lvaldecl(stderr_ref, x0)
//
implement
fprint_lvaldecl
  (out, x0) = let
//
val+LVALDECL(rcd) = x0
//
in
  fprint!
  ( out
  , "LVALDECL@{"
  , ", pat=", rcd.pat
  , ", def=", rcd.def
  , ", def_blk=", rcd.def_blk, "}")
end // end of [fprint_lvaldecl]
//
(* ****** ****** *)
//
implement
print_lvardecl(x0) =
fprint_lvardecl(stdout_ref, x0)
implement
prerr_lvardecl(x0) =
fprint_lvardecl(stderr_ref, x0)
//
implement
fprint_lvardecl
  (out, x0) = let
//
val+LVARDECL(rcd) = x0
//
in
  fprint!
  ( out
  , "LVARDECL@{", rcd.loc, "}")
(*
  fprint!
  ( out
  , "LVARDECL@{"
  , ", pat=", rcd.pat
  , ", def=", rcd.def
  , ", def_blk=", rcd.def_blk, "}")
*)
end // end of [fprint_lvardecl]
//
(* ****** ****** *)
//
implement
print_limpdecl3(x0) =
fprint_limpdecl3(stdout_ref, x0)
implement
prerr_limpdecl3(x0) =
fprint_limpdecl3(stderr_ref, x0)
//
implement
fprint_limpdecl3
  (out, x0) = let
//
val+LIMPDECL3(rcd) = x0
//
in
//
fprint!
( out
, "LIMPDECL3@{"
, "hdc=", rcd.hdc, "; "
, "hag=", rcd.hag, "; "
, "def=", rcd.def, "; "
, "lev=(", rcd.lev, "); "
, "lts=(", rcd.lts, "); "
, "hag_blk=", rcd.hag_blk, "; "
, "def_blk=", rcd.def_blk, "; ", "}")
//
end // end of [fprint_limpdecl3]
//
(* ****** ****** *)

(* end of [xats_intrep1_print.dats] *)
