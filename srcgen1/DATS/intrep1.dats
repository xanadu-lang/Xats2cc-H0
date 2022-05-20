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
typedef
slabeled
(a:type) = $S2E.slabeled(a)
(* ****** ****** *)

fun
h0typ_size
(h0t0: h0typ): int =
(
  auxh0t0(h0t0)) where
{
fun
auxh0t0
(h0t0: h0typ): int =
(
case+
h0t0.node() of
| H0Tbas _ => 1
//
| H0Tcst _ => 1
| H0Tvar _ => 1
//
|
H0Tlft(h0t1) => auxh0t0(h0t1) + 1
//
|
H0Tfun
( npf0
, h0ts, h0t1) =>
auxh0ts(h0ts) + auxh0t0(h0t1) + 1
//
|
H0Tapp
(h0t1, h0ts) =>
auxh0t0(h0t1) + auxh0ts(h0ts) + 1
|
H0Tlam
(htvs, h0t1) => auxh0t0(h0t1) + 1
//
|
H0Ttyext
(name, h0ts) => auxh0ts(h0ts) + 1
//
|
H0Ttyrec
( knd0
, npf1, lhts) => auxlhts(lhts) + 1
//
|
H0Tnone0 _ => (0) | H0Tnone1 _ => (0)
) (*case*) // end of [auxh0t0]
//
and
auxh0ts
(h0ts: h0typlst): int =
(
case+ h0ts of
|
list_nil() => 0
|
list_cons
(h0t1, h0ts) =>
(
  auxh0t0( h0t1 ) + auxh0ts( h0ts )
)
) (*case*) // end of [auxh0ts]
//
and
auxlhts
(lhts: labh0typlst): int =
(
case+ lhts of
|
list_nil() => 0
|
list_cons
(lht1, lhts) =>
let
val+
$S2E.SLABELED(l1, h0t1) = lht1
in
  auxh0t0( h0t1 ) + auxlhts( lhts )
end
) (*case*) // end of [auxlhts]
//
} (*where*) // end of [ h0typ_size ]

(* ****** ****** *)

local
//
typedef
l1tnm_struct =
@{
  l1tnm_htsz= (int)
, l1tnm_type= h0typ
, l1tnm_lctp= l1ctp
, l1tnm_stamp= stamp
} // end of [l1tnm_struct]
//
absimpl
l1tnm_tbox = ref(l1tnm_struct)
//
(* ****** ****** *)
in(* in-of-local *)
(* ****** ****** *)

implement
l1tnm_make_type
  ( h0t0 ) =
(
case+ opt1 of
| ~
None_vt() => ltnm where
{
//
val
stmp =
l1tnm_stamp_new()
//
val
ltnm =
ref<l1tnm_struct>
(
@{
, l1tnm_htsz=htsz
, l1tnm_type=h0t0
, l1tnm_lctp=lctp
, l1tnm_stamp=stmp
}
) where
{
  val htsz =
  h0typ_size(h0t0)
  val lctp = L1CTPnone()
}
//
val () =
the_ltnmmap_insert_exn(h0t0, ltnm)
}
| ~Some_vt(ltnm) => ltnm(*already*)
//
) where
{
val
opt1 = the_ltnmmap_search_opt(h0t0)
} // end of [l1tnm_make_type]

(* ****** ****** *)
//
implement
l1tnm_get_htsz(x0) = x0->l1tnm_htsz
implement
l1tnm_get_type(x0) = x0->l1tnm_type
implement
l1tnm_get_lctp(x0) = x0->l1tnm_lctp
implement
l1tnm_get_stamp(x0) = x0->l1tnm_stamp
//
(*
implement
l1tnm_set_kind
(x0, knd1) =
(x0->l1tnm_kind := knd1)
*)
implement
l1tnm_set_lctp
(x0, lctp) =
(x0->l1tnm_lctp := lctp) where
{
(*
  val () = ( x0->l1tnm_kind := (1) )
*)
} (*where*)//end-of-[l1tnm_set_lctp]
//
(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
implement
l1tnm_none0() =
(
  l1tnm_make_type(h0typ_none0())
)
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
//
implement
eq_l1tmp_l1tmp
  (x1, x2) =
let
val stmp1 = x1.stamp()
and stmp2 = x2.stamp()
in
$STM.eq_stamp_stamp(stmp1, stmp2)
end (*let*) // end of [eq_l1tmp_l1tmp]
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
, l1tmp_ltnm= l1tnm // layout type
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
ltnm =
l1tnm_none0()
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
, l1tmp_ltnm= ltnm(*layout*)
, l1tmp_stamp= stamp(*unicity*)
} end // end-of-[l1tmp_new_tmp]

implement
l1tmp_new_arg
( loc
, idx ) =
let
val
l1tp =
l1tnm_none0()
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
, l1tmp_ltnm= l1tp(*layout*)
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
l1tmp_get_ltnm
  (tmp) =
  tmp->l1tmp_ltnm
implement
l1tmp_set_ltnm
  (tmp, ltnm) =
(
  tmp->l1tmp_ltnm := ltnm
)
//
implement
l1tmp_get_stamp(tmp) = tmp->l1tmp_stamp
//
(* ****** ****** *)

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

(* ****** ****** *)
in(* in-of-local *)
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
