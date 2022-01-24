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

local
val
l1tp =
l1typ_make_node
(L1TYPnone0(*void*))
in // in-of-local
implement
l1typ_none0() = l1tp
end // end of [local]

(* ****** ****** *)

local

absimpl
l1typ_tbox = $rec
{ 
l1typ_node= l1typ_node
} (* end of [absimpl] *)

in (*in-of-local*)

(* ****** ****** *)
//
implement
l1typ_make_node
  (node) =
(
  $rec{ l1typ_node=node }
)
//
(* ****** ****** *)

implement
l1typ_get_node(x0) = x0.l1typ_node

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

val
stamper =
$STM.stamper_new()

in (* in-of-local *)

implement
l1exn_stamp_new() =
$STM.stamper_getinc(stamper)

end // end of [local]

(* ****** ****** *)

local

absimpl
l1exn_tbox = $rec
{
  l1exn_loc= loc_t // location
, l1exn_stamp= stamp (* unicity *)
} // end of [l1exn]

in (* in-of-local *)

implement
l1exn_new_loc
  (  loc  ) =
$rec
{ l1exn_loc= loc
, l1exn_stamp= stamp(*unicity*)
} where
{
  val stamp = l1exn_stamp_new()
}

(* ****** ****** *)
implement
l1exn_get_loc(exn) = exn.l1exn_loc
(* ****** ****** *)
implement
l1exn_get_stamp(exn) = exn.l1exn_stamp
(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

val
stamper =
$STM.stamper_new()

in (* in-of-local *)

implement
l1tmp_stamp_new() =
$STM.stamper_getinc(stamper)

end // end of [local]

(* ****** ****** *)
//
implement
eq_l1tmp_l1tmp
  (x1, x2) =
(
$STM.eq_stamp_stamp
(
  x1.stamp(), x2.stamp()
)
) (* end of [eq_l1tmp_l1tmp] *)
//
(* ****** ****** *)

local

(* ****** ****** *)

typedef
l1tmp_struct =
@{
  l1tmp_loc= loc_t
, l1tmp_arg= int // 0/1 : let/arg
, l1tmp_ref= int // 0/1 : val/ref
, l1tmp_ret= int // return status
, l1tmp_lev= int // function level
, l1tmp_type= l1typ // layout type
, l1tmp_stamp= stamp // for unicity
} // end of [l1tmp]
absimpl
l1tmp_tbox = ref(l1tmp_struct)

(* ****** ****** *)

in (* in of [local] *)

(* ****** ****** *)

implement
l1tmp_new_tmp
  (loc) =
let
val
l1tp =
l1typ_none0()
val
stamp =
l1tmp_stamp_new()
in
ref<l1tmp_struct>
@{
  l1tmp_loc= loc
, l1tmp_arg= 0(*let*)
, l1tmp_ref= 0(*val*)
, l1tmp_ret= 0(*nret*)
, l1tmp_lev= ~1 // uninited
, l1tmp_type= l1tp(*layout*)
, l1tmp_stamp= stamp(*unicity*)
} end // end-of-[l1tmp_new_tmp]

implement
l1tmp_new_arg
( loc
, idx ) =
let
val
l1tp =
l1typ_none0()
val
stamp =
l1tmp_stamp_new()
in
ref<l1tmp_struct>
@{
  l1tmp_loc= loc
, l1tmp_arg= idx // idx >= 1
, l1tmp_ref= 0(*val*)
, l1tmp_ret= 0(*nret*)
, l1tmp_lev= ~1 // uninitied
, l1tmp_type= l1tp(*layout*)
, l1tmp_stamp= stamp(*unicity*)
} end // end of [l1tmp_new_arg]

(* ****** ****** *)
//
implement
l1tmp_get_loc(tmp) = tmp->l1tmp_loc
implement
l1tmp_get_arg(tmp) = tmp->l1tmp_arg
implement
l1tmp_get_ref(tmp) = tmp->l1tmp_ref
//
implement
l1tmp_get_lev
  (tmp) =
  tmp->l1tmp_lev
implement
l1tmp_set_lev
  (tmp, lev) =
(
  tmp->l1tmp_lev := lev
)
//
implement
l1tmp_get_type
  (tmp) =
  tmp->l1tmp_type
implement
l1tmp_set_type
  (tmp, l1t) =
(
  tmp->l1tmp_type := l1t
)
//
implement
l1tmp_get_stamp(tmp) = tmp->l1tmp_stamp
//
(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

val
stamper =
$STM.stamper_new()

in (* in-of-local *)

implement
l1cst_stamp_new() =
$STM.stamper_getinc(stamper)

end // end of [local]

(* ****** ****** *)

local

absimpl
l1cst_tbox = $rec
{
  l1cst_loc= loc_t // location
, l1cst_hdc= hdcst // original
, l1cst_stamp= stamp (* unicity *)
} // end of [l1cst]

in (* in-of-local *)

implement
l1cst_new_hdc
  (loc, hdc) =
$rec
{
  l1cst_loc= loc
, l1cst_hdc= hdc
, l1cst_stamp= stamp(*unicity*)
} where
{
  val stamp = l1cst_stamp_new()
}
(* ****** ****** *)

implement
l1cst_get_loc(l1c) = l1c.l1cst_loc
implement
l1cst_get_hdc(l1c) = l1c.l1cst_hdc
implement
l1cst_get_stamp(l1c) = l1c.l1cst_stamp

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

absimpl
l1val_tbox = $rec
{ l1val_loc= loc_t
, l1val_node= l1val_node
} (* end of [absimpl] *)

in

(* ****** ****** *)

implement
l1val_make_node
  (loc, node) = $rec
{
  l1val_loc=loc, l1val_node=node
}

(* ****** ****** *)

implement
l1val_get_loc(x0) = x0.l1val_loc
implement
l1val_get_node(x0) = x0.l1val_node

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
//
(*
implement
l1val_exn(exn) =
l1val_make_node
(exn.loc(), L1VALexn(exn))
*)
//
implement
l1val_tmp(tmp) =
l1val_make_node
(tmp.loc(), L1VALtmp(tmp))
//
(* ****** ****** *)
//
implement
l1val_flat(l1v) =
l1val_make_node
(l1v.loc(), L1VALflat(l1v))
//
implement
l1val_addr(l1v) =
l1val_make_node
(l1v.loc(), L1VALaddr(l1v))
implement
l1val_talf(l1v) =
l1val_make_node
(l1v.loc(), L1VALtalf(l1v))
//
(* ****** ****** *)
//
implement
l1val_ctag
(loc, l1v) =
l1val_make_node
(loc, L1VALctag(l1v))
implement
l1val_carg
(loc, l1v, idx) =
l1val_make_node
(loc, L1VALcarg(l1v, idx))
implement
l1val_cofs
(loc, l1v, idx) =
l1val_make_node
(loc, L1VALcofs(l1v, idx))
//
(* ****** ****** *)
//
implement
l1val_targ
(loc, l1v, idx) =
l1val_make_node
(loc, L1VALtarg(l1v, idx))
implement
l1val_tptr
(loc, l1v, idx) =
l1val_make_node
(loc, L1VALtptr(l1v, idx))
//
(* ****** ****** *)

local

absimpl
l1cmd_tbox = $rec
{ l1cmd_loc= loc_t
, l1cmd_node= l1cmd_node
} (* end of [absimpl] *)

in

implement
l1cmd_make_node
  (loc, node) = $rec
{
  l1cmd_loc=loc, l1cmd_node=node
}

(* ****** ****** *)

implement
l1cmd_get_loc(x0) = x0.l1cmd_loc
implement
l1cmd_get_node(x0) = x0.l1cmd_node

end // end of [local]

(* ****** ****** *)

implement
l1blk_none() = L1BLKnone()
implement
l1blk_some(xs) = L1BLKsome(xs)

(* ****** ****** *)

local

absimpl
l1dcl_tbox = $rec
{ l1dcl_loc= loc_t
, l1dcl_node= l1dcl_node
} (* end of [absimpl] *)

in(*in-of-local*)

implement
l1dcl_make_node
  (loc, node) = $rec
{
  l1dcl_loc=loc, l1dcl_node=node
}

(* ****** ****** *)

implement
l1dcl_get_loc(x0) = x0.l1dcl_loc
implement
l1dcl_get_node(x0) = x0.l1dcl_node

end // end of [local]

(* ****** ****** *)

(* end of [xats_intrep1.dats] *)
