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
fprint_val<l1tmp> = fprint_l1tmp
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
(out, "arg[", arg, "](", x0.stamp(), ")")
//
end // end of [fprint_l1tmp]
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
  fprint!
  ( out
  , "LFUNDECL@{", rcd.loc, "}")
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
  , ", pat=", rcd.pat, "}")
(*
  , ", def=", rcd.def
  , ", def_blk=", rcd.def_blk, "}")
*)
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

(* end of [xats_intrep1_print.dats] *)
