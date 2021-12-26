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
#staload "./../SATS/xcomp01.sats"
(* ****** ****** *)

local

(*
#define VARG 0 // arg. vars
#define VLOC 0 // local vars
#define VENV 1 // environ. vars
*)
#define VFIX 2 // fixed binding
(*
#define VTOP %(~1) // top-level vars
*)

fun
auxval_var
( env0:
! compenv
, h0e0: h0exp): l1val =
let
//
val 
loc0 = h0e0.loc()
val-
H0Evar(x0) = h0e0.node()
//
val
opt0 = xcomp01_dvarfind(env0, x0)
in
//
case+ opt0 of
| ~
Some_vt(l1v1) => l1v1
| ~
None_vt((*void*)) =>
l1val_make_node(loc0, L1VALvfix(x0))
//
end // end of [auxval_var]

in(*in-of-local*)

implement
xcomp01_h0exp_val
  (env0, h0e0) =
let
val loc0 = h0e0.loc()
in(*in-of-let*)
//
case+
h0e0.node() of
//
| H0Ei00(int) =>
  l1val_make_node
  (loc0, L1VALi00(int))
| H0Eb00(btf) =>
  l1val_make_node
  (loc0, L1VALb00(btf))
| H0Es00(str) =>
  l1val_make_node
  (loc0, L1VALs00(str))
//
| H0Eint(tok) =>
  l1val_make_node
  (loc0, L1VALint(tok))
| H0Ebtf(tok) =>
  l1val_make_node
  (loc0, L1VALbtf(tok))
| H0Echr(tok) =>
  l1val_make_node
  (loc0, L1VALchr(tok))
//
| H0Etop(tok) =>
  l1val_make_node
  (loc0, L1VALtop(tok))
//
| H0Evar _ =>
  auxval_var(env0, h0e0)
(*
| H0Ekvar _ =>
  auxval_kvar(env0, h0e0)
*)
//
| _ (* rest-of-h0exp *) =>
(
l1val_make_node(loc0, L1VALnone1(h0e0))
)
//
end // end of [xcomp01_h0exp_val]

end // end of [local]

(* ****** ****** *)

implement
xcomp01_package
  (h0pkg) =
(
  L1PKG
  (ltmps, ldcls)
) where
{
//
(*
val () =
xcomp01_initize()
*)
//
val
env0 =
compenv_make_nil()
//
val+
H0COMPED(rcd) = h0pkg
//
val hdcls =
(
case+
rcd.comped of
| None() =>
  list_nil((*void*))
| Some(hdcls) => hdcls
) : h0dclist // end-of-val
//
val
ldcls =
xcomp01_h0dclist_dcl(env0, hdcls)
//
val ltmps = compenv_free_top(env0)
//
} (* end of [xcomp01_package] *)

(* ****** ****** *)

local

(* ****** ****** *)

fun
aux_fundecl
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
//
val
loc0 = dcl0.loc()
//
val-
H0Cfundecl
( knd0
, mopt
, tqas
, hfds) = dcl0.node()
//
val () =
xcomp01_dvaradd_fun0(env0)
//
in
case+ tqas of
| list_nil() => // function
  aux_fundecl_fun(env0, dcl0)
| list_cons _ => // template
  aux_fundecl_tmp(env0, dcl0)
end // end of [aux_fundecl]

and
aux_fundecl_fun
( env0:
! compenv
, dcl0: h0dcl )
: l1dcl =
let
val () =
let
(*
HX: for recursion
*)
fun
auxlst_bind
( env0
: !compenv
, hfds
: hfundeclist): void =
(
case+ hfds of
|
list_nil() => ()
|
list_cons
(hfd1, hfds) =>
let
  val+
  HFUNDECL
  ( rcd ) = hfd1
  val loc = rcd.loc
  val nam = rcd.nam
  val hdc = rcd.hdc
  val
  itm =
  l1val_make_node
  (loc, L1VALfcst(hdc))
  val () =
  xcomp01_dvaradd_bind
  (env0, nam, itm(*l1val*))
in
  auxlst_bind( env0, hfds )
end
) (* end of [auxlst_bind] *)
//
in
  auxlst_bind( env0, hfds )
end
//
val
lfds =
xcomp01_hfundeclist(env0, hfds)
//
val () = xcomp01_dvarpop_fun0(env0)
//
in
l1dcl_make_node
(loc0, L1DCLfundecl(knd0, mopt, lfds))
//
end where
{
//
val
loc0 = dcl0.loc()
val-
H0Cfundecl
( knd0
, mopt
, tqas, hfds) = dcl0.node()
//
(*
val () =
println!
("aux_fundecl_fun: exit(1)")
val ((*exit*)) = exit_void(1)
*)
//
} (* end of [aux_fundecl_fun] *)

and
aux_fundecl_tmp
( env0:
! compenv
, dcl0: h0dcl )
: l1dcl =
let
//
val loc0 = dcl0.loc()
(*
HX: should template be compiled?
*)
in
l1dcl_make_node(loc0, L1DCLnone0())
end // end of [aux_fundecl_tmp]

(* ****** ****** *)

fun
aux_valdecl
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
val
loc0 = dcl0.loc()
val-
H0Cvaldecl
( knd0
, mopt, hvds) = dcl0.node()
val
lvds =
xcomp01_hvaldeclist(env0, hvds)
in
l1dcl_make_node
(loc0, L1DCLvaldecl(knd0, mopt, lvds))
end // end of [aux_valdecl]

(* ****** ****** *)

fun
aux_vardecl
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
val
loc0 = dcl0.loc()
val-
H0Cvardecl
( knd0
, mopt, hvds) = dcl0.node()
val
lvds =
xcomp01_hvardeclist(env0, hvds)
in
l1dcl_make_node
(loc0, L1DCLvardecl(knd0, mopt, lvds))
end // end of [aux_vardecl]

(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

implement
xcomp01_h0dcl_dcl
  (env0, dcl0) =
let
(*
val
loc0 = dcl0.loc()
*)
in
//
case+
dcl0.node() of
//
|
H0Cfundecl _ =>
aux_fundecl(env0, dcl0)
//
|
H0Cvaldecl _ =>
aux_valdecl(env0, dcl0)
|
H0Cvardecl _ =>
aux_vardecl(env0, dcl0)
//
|
_ (* else *) =>
let
  val loc0 = dcl0.loc()
in
l1dcl_make_node(loc0, L1DCLnone1(dcl0))
end
//
end // end of [xcomp01_h0dcl_dcl]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
implement
xcomp01_h0dclist_dcl
  (env0, dcls) =
(
case+ dcls of
|
list_nil() =>
list_nil()
|
list_cons(dcl1, dcls) =>
let
val dcl1 =
xcomp01_h0dcl_dcl(env0, dcl1)
in
  list_cons(dcl1, dcls) where
{
val
dcls = xcomp01_h0dclist_dcl(env0, dcls)
}
end
) (* end of [xcomp01_h0dclist_dcl] *)
(* ****** ****** *)

implement
xcomp01_hfundecl
(env0, dcl0) =
let
//
val+
HFUNDECL
( rcd ) = dcl0
//
val loc = rcd.loc
val nam = rcd.nam
val hdc = rcd.hdc
//
in
  LFUNDECL@{
    loc=loc
  , nam=nam, hdc=hdc
  }
end (*let*) // end of [xcomp01_hfundecl]

(* ****** ****** *)

local
(*
fun
isdecl
( hfd
: hfundecl): bool =
let
val+HFUNDECL(rcd) = hfd
in
case+ rcd.def of
| None _ => true | Some _ => false
end // end of [isdecl]
*)
in(*in-of-local*)
//
implement
xcomp01_hfundeclist
  (env0, xs) =
(
case+ xs of
|
list_nil() =>
list_nil()
|
list_cons(x0, xs) =>
list_cons(x0, xs) where
{
val x0 = xcomp01_hfundecl(env0, x0)
val xs = xcomp01_hfundeclist(env0, xs)
}
) (* end of [xcomp01_hfundeclist] *)
//
end // end of [local]

(* ****** ****** *)

implement
xcomp01_hvaldecl
  (env0, dcl0) =
let
//
val+
HVALDECL
( rcd ) = dcl0
//
val loc = rcd.loc
val pat = rcd.pat
val def = rcd.def
//
var res
  : l1valopt = None()
//
val blk =
(
case+ def of
|
None() => l1blk_none()
|
Some(h0e1) =>
(
xcomp01_lcmdpop0_blk(env0)
) where
{
val () =
xcomp01_lcmdpush_nil(env0)
//
val
l1v1 =
xcomp01_l1valize
  (env0, l1v1) where
{
val
l1v1 =
xcomp01_h0exp_val(env0, h0e1)
}
val () = ( res := Some(l1v1) )
//
(*
val () =
xcomp01_h0pat_ck01(env0, pat, l1v1)
*)
//
} (* end of [Some] *)
) : l1blk // end of [val]
//
in
  LVALDECL@{
    loc=loc
  , pat=pat, def=res, def_blk=blk
  } (* LVALDECL *)
end
(*let*) // end of [xcomp01_hvaldecl]

(* ****** ****** *)

implement
xcomp01_hvaldeclist
  (env0, xs) =
(
case+ xs of
|
list_nil() =>
list_nil()
|
list_cons(x0, xs) =>
list_cons(x0, xs) where
{
val x0 = xcomp01_hvaldecl(env0, x0)
val xs = xcomp01_hvaldeclist(env0, xs)
}
) (* end of [xcomp01_hvaldeclist] *)

(* ****** ****** *)

implement
xcomp01_hvardecl
(env0, dcl0) =
let
//
val+
HVARDECL
( rcd ) = dcl0
//
val loc = rcd.loc
//
in
  LVARDECL@{ loc=loc }
end (*let*) // end of [xcomp01_hvardecl]

(* ****** ****** *)

implement
xcomp01_hvardeclist
  (env0, xs) =
(
case+ xs of
|
list_nil() => list_nil()
|
list_cons(x0, xs) =>
list_cons(x0, xs) where
{
val x0 = xcomp01_hvardecl(env0, x0)
val xs = xcomp01_hvardeclist(env0, xs)
}
) (* end of [xcomp01_hvardeclist] *)

(* ****** ****** *)

(* end of [xats_xcomp01_dynexp.dats] *)
