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
//
macdef
xltmpnew_tmp0 =
xcomp01_ltmpnew_tmp0
macdef
xltmpnew_arg1 =
xcomp01_ltmpnew_arg1
//
(* ****** ****** *)

implement
xcomp01_hdcon
  (env0, hdc) =
let
//
val
tag = hdc.tag()
//
in
//
if
(tag >= 0)
then LDCONcon(hdc)
else
let
//
val hdv =
hdcon_get_dvar(hdc)
val opt =
xcomp01_dvarfind(env0, hdv)
//
(*
val () =
println!
("auxval_fcon: hdc = ", hdc)
val () =
println!
("auxval_fcon: hdv = ", hdv)
*)
//
in
case- opt of
~Some_vt(l1v) => LDCONval(l1v)
end // end of [else]
//
end // end of [xcomp01_hdcon]

(* ****** ****** *)

local

(* ****** ****** *)

fun
auxdap1
( env0
: !compenv
, h0p0
: h0pat
, l1v1
: l1val): l1pck =
let
val-
H0Pdap1
( h0f0) = h0p0.node()
in
xcomp01_h0pat_ck0
(env0, h0f0, ltag) where
{
val
ltag =
l1val_ctag(l1v1.loc(), l1v1)
}
end // end of [auxdap1]

(* ****** ****** *)

fun
auxdapp
( env0
: !compenv
, h0p0
: h0pat
, l1v1
: l1val): l1pck =
let
val-
H0Pdapp
( h0f0
, npf1
, h0ps) = h0p0.node()
//
val
ltag =
l1val_ctag
(l1v1.loc(), l1v1)
val
pckf =
xcomp01_h0pat_ck0
( env0, h0f0, ltag )
//
in
L1PCKapp
( pckf
, auxnps(env0, npf1, h0ps))
end where
{
//
fun
auxnps
( env0:
! compenv
, npf1: int
, h0ps
: h0patlst): l1pcklst =
(
case+ h0ps of
|
list_nil() =>
list_nil()
|
list_cons _ =>
if
(npf1 > 0)
then
let
val
npf1 = npf1 - 1
val-
list_cons
(_, h0ps) = h0ps
in
auxnps
(env0, npf1, h0ps)
end
else
auxlst
(env0, h0ps, 0(*idx0*))
) (* end of [auxnps] *)
//
and
auxlst
( env0:
! compenv
, h0ps
: h0patlst
, idx0: int): l1pcklst =
(
case+ h0ps of
|
list_nil() => list_nil()
|
list_cons
(h0p1, h0ps) =>
let
val
loc1 = l1v1.loc()
val
arg1 =
l1val_carg
(loc1, l1v1, idx0)
val
pck1 = 
xcomp01_h0pat_ck0
(env0, h0p1, arg1)
in
list_cons(pck1, pcks) where
{
val
pcks = auxlst(env0, h0ps, idx0+1)
}
end
) (* end of [auxlst] *)
//
} (* end of [auxdapp] *)

(* ****** ****** *)

fun
aux_tuple
( env0
: !compenv
, h0p0
: h0pat
, l1v1
: l1val): l1pck =
let
val-
H0Ptuple
( knd0
, npf1
, h0ps) = h0p0.node()
//
fun
auxnps
( env0:
! compenv
, npf1: int
, h0ps
: h0patlst): l1pcklst =
(
case+ h0ps of
|
list_nil() =>
list_nil()
|
list_cons _ =>
if
(npf1 > 0)
then
let
val
npf1 = npf1 - 1
val-
list_cons
(_, h0ps) = h0ps
in
auxnps
(env0, npf1, h0ps)
end
else
auxlst
(env0, h0ps, 0(*idx0*))
) (* end of [auxnps] *)
//
and
auxlst
( env0:
! compenv
, h0ps
: h0patlst
, idx0: int): l1pcklst =
(
case+ h0ps of
|
list_nil() => list_nil()
|
list_cons
(h0p1, h0ps) =>
let
val
loc1 = l1v1.loc()
val
arg1 =
(
if
(knd0 > 0)
then
l1val_carg
(loc1, l1v1, idx0)
else
l1val_targ
(loc1, l1v1, idx0)): l1val
//
val
pck1 = 
xcomp01_h0pat_ck0
(env0, h0p1, arg1)
//
in
list_cons(pck1, pcks) where
{
val
pcks =
auxlst(env0, h0ps, idx0+1)
}
end
) (* end of [auxlst] *)
//
in
L1PCKtup
(knd0, auxnps(env0, npf1, h0ps))
end (* end of [aux_tuple] *)

in(*in-of-local*)

implement
xcomp01_h0pat_ck0
( env0
, h0p0, l1v1) =
let
val
loc0 = h0p0.loc()
in
//
case+
h0p0.node() of
//
|
H0Pany _ => L1PCKany()
|
H0Pvar _ => L1PCKany()
//
|
H0Pi00
( int1 ) =>
(
  L1PCKi00(int1, l1v1)
)
|
H0Pint
( tok1 ) =>
(
  L1PCKint(tok1, l1v1)
)
|
H0Pbtf
( tok1 ) =>
(
  L1PCKbtf(tok1, l1v1)
)
|
H0Pchr
( tok1 ) =>
(
  L1PCKchr(tok1, l1v1)
)
|
H0Pstr
( tok1 ) =>
(
  L1PCKstr(tok1, l1v1)
)
//
|
H0Pbang(h0p1) =>
xcomp01_h0pat_ck0
( env0, h0p1(*var*), l1v1 )
|
H0Pflat(h0p1) =>
xcomp01_h0pat_ck0
( env0, h0p1(*con*), l1v1 )
|
H0Pfree(h0p1) =>
xcomp01_h0pat_ck0
( env0, h0p1(*con*), l1v1 )
//
|
H0Pcon( hdc1 ) =>
let
val ldc1 =
xcomp01_hdcon(env0, hdc1)
in
  L1PCKcon(ldc1, l1v1(*ctag*))
end
//
|
H0Pdap1 _ =>
(
auxdap1
(env0, h0p0, l1v1(*con-val*))
)
//
|
H0Pdapp _ =>
(
auxdapp
(env0, h0p0, l1v1(*con-val*))
)
//
|
H0Ptuple _ =>
(
aux_tuple
(env0, h0p0, l1v1(*tup-val*))
)
//
|
_ (*else*) => L1PCKxpat(h0p0, l1v1)
//
end // end of [xcomp01_h0pat_ck0]

end // end of [local]

(* ****** ****** *)

local

fun
auxvar
( env0:
! compenv
, h0p0: h0pat
, l1v1: l1val): void =
(
xcomp01_dvaradd_bind
( env0, hdv0, l1v1 )
) where
{
(*
val loc0 = h0p0.loc()
*)
val-
H0Pvar(hdv0) = h0p0.node()
} (* end of [auxvar] *)

(* ****** ****** *)

local

fun
auxtalf
( l1v0
: l1val): l1val =
(
case+
l1v0.node() of
|
L1VALcarg
(l1v1, idx2) => //boxed!
let
  val loc0 = l1v0.loc( )
in
  l1val_cofs(loc0, l1v1, idx2)
end
//
| _(* else *) => l1val_talf(l1v0)
)

in(* in-of-local *)

fun
auxbang
( env0:
! compenv
, h0p0: h0pat
, l1v1: l1val): void =
let
(*
val loc0 = h0p0.loc()
*)
val-
H0Pbang
( h0p1 ) = h0p0.node()
//
in
case+
h0p1.node() of
|
H0Pvar _ =>
let
val
lptr = auxtalf(l1v1)
//
(*
val () =
println!
("auxbang: H0Pvar: lptr = ", lptr)
*)
//
in
xcomp01_h0pat_ck1(env0, h0p1, lptr)
end // end of [H0Pvar]
| _ (* else *) =>
xcomp01_h0pat_ck1(env0, h0p1, l1v1)
end (*let*) // end of [auxbang]

end // end of [local]

(* ****** ****** *)

fun
auxflat
( env0:
! compenv
, h0p0: h0pat
, l1v1: l1val): void =
let
(*
val loc0 = h0p0.loc()
*)
val-
H0Pflat
( h0p1 ) = h0p0.node()
in
xcomp01_h0pat_ck1(env0, h0p1, l1v1)
end (*let*) // end of [auxflat]

(* ****** ****** *)

fun
auxfree
( env0:
! compenv
, h0p0: h0pat
, l1v1: l1val): void =
let
(*
val loc0 = h0p0.loc()
*)
val-
H0Pfree
( h0p1 ) = h0p0.node()
in
xcomp01_h0pat_ck1(env0, h0p1, l1v1)
end (*let*) // end of [auxfree]

(* ****** ****** *)

fun
auxdapp
( env0:
! compenv
, h0p0: h0pat
, l1v1: l1val): void =
let
//
val
loc0 = h0p0.loc()
//
val-
H0Pdapp
( h0f0
, npf1
, h0ps) = h0p0.node()
//
in
let
//
fun
auxh0ps
( env0:
! compenv
, npf1: int
, h0ps
: h0patlst
, idx0: int): void =
(
case+ h0ps of
|
list_nil() => ()
|
list_cons
(h0p1, h0ps) =>
if
npf1 > 0
then
let
val npf1 = npf1-1
in
auxh0ps
(env0, npf1, h0ps, idx0)
end // end of [then]
else
let
val
loc1 =
l1v1.loc()
val
carg =
l1val_carg
(loc1, l1v1, idx0)
val () =
xcomp01_h0pat_ck1
(env0, h0p1, carg)
in
let
val idx0 = idx0+1
in
auxh0ps
(env0, npf1, h0ps, idx0)
end
end // end of [else]
) (* end of [auxh0ps] *)
//
in
auxh0ps
(env0, npf1, h0ps, 0(*idx0*))
end // end of [let]
//
end (*let*) // end of [auxdapp]

(* ****** ****** *)

fun
aux_tuple
( env0:
! compenv
, h0p0: h0pat
, l1v1: l1val): void =
let
//
val
loc0 = h0p0.loc()
//
val-
H0Ptuple
( knd0
, npf1
, h0ps) = h0p0.node()
//
in
let
//
fun
auxh0ps
( env0:
! compenv
, npf1: int
, h0ps
: h0patlst
, idx0: int): void =
(
case+ h0ps of
|
list_nil() => ()
|
list_cons
(h0p1, h0ps) =>
if
npf1 > 0
then
let
val npf1 = npf1-1
in
auxh0ps
(env0, npf1, h0ps, idx0)
end // end of [then]
else
let
val
loc1 = l1v1.loc()
val
arg1 =
(
if
(knd0 > 0)
then
l1val_carg
(loc1, l1v1, idx0)
else
l1val_targ
(loc1, l1v1, idx0)): l1val
//
val () =
xcomp01_h0pat_ck1
(env0, h0p1, arg1)
//
in
auxh0ps
(env0, npf1, h0ps, idx0+1)
end // end of [else]
) (* end of [auxh0ps] *)
//
in
auxh0ps
(env0, npf1, h0ps, 0(*idx0*))
end // end of [let]
//
end (*let*) // end of [aux_tuple]

(* ****** ****** *)

in(*in-of-local*)

implement
xcomp01_h0pat_ck1
( env0
, h0p0, l1v1) =
let
val
loc0 = h0p0.loc()
in(*in-of-let*)
//
case+
h0p0.node() of
//
| H0Pany _ => ()
//
| H0Pvar _ =>
  auxvar(env0, h0p0, l1v1)
//
| H0Pbang _ =>
  auxbang(env0, h0p0, l1v1)
| H0Pflat _ =>
  auxflat(env0, h0p0, l1v1)
| H0Pfree _ =>
  auxfree(env0, h0p0, l1v1)
//
| H0Pdap1 _ => ()
| H0Pdapp _ =>
  auxdapp(env0, h0p0, l1v1)
//
| H0Ptuple _ =>
  aux_tuple(env0, h0p0, l1v1)
//
|
_ (* else *) =>
let
val lcmd =
l1cmd_make_node
(loc0, L1CMDmatch(h0p0, l1v1))
in
xcomp01_lcmdadd_lcmd(env0, lcmd)
end
//
end // end of [xcomp01_h0pat_ck1]

end // end of [local]

(* ****** ****** *)

implement
xcomp01_h0pat_ck01
(env0, h0p0, l1v1) =
let
val
loc0 = h0p0.loc()
val
lpck =
xcomp01_h0pat_ck0
(env0, h0p0, l1v1)
val
lcmd =
l1cmd_make_node
(loc0, L1CMDpatck(lpck))
val () =
xcomp01_lcmdadd_lcmd(env0, lcmd)
//
in
xcomp01_h0pat_ck1(env0, h0p0, l1v1)
end // end of [xcomp01_h0pat_ck01]

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

(* ****** ****** *)

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

fun
auxval_kvar
( env0:
! compenv
, h0e0: h0exp): l1val =
let
//
val 
loc0 = h0e0.loc()
val-
H0Ekvar
(k0, x0) = h0e0.node()
//
in
//
ifcase
|
k0 = VFIX =>
l1val_make_node
( loc0, L1VALvfix(x0) )
|
_ (* else *) =>
let
val
opt0 =
xcomp01_dvarfind(env0, x0)
//
in
//
case+ opt0 of
| ~
Some_vt(l1v1) => l1v1
| ~
None_vt((*void*)) =>
l1val_make_node(loc0, L1VALnone1(h0e0))
//
end // end-of-else
end // end of [auxval_kvar]

(* ****** ****** *)

fun
auxset_dapp
( env0:
! compenv
, h0e0: h0exp
, tres: l1tmp): void =
let
//
val
loc0 = h0e0.loc()
val-
H0Edapp
( h0f0
, npf1
, h0es) = h0e0.node()
//
val () =
xcomp01_lcmdpush_nil(env0)
//
in
let
val
lapp =
l1cmd_make_node
( loc0
, L1CMDapp(tres, l1f0, l1vs))
val () =
xcomp01_lcmdadd_lcmd(env0, lapp)
//
val lblk =
l1cmd_make_node
(loc0, L1CMDblk(blk0)) where
{
val
blk0 = xcomp01_lcmdpop0_blk(env0)
}
in
  xcomp01_lcmdadd_lcmd(env0, lblk)
end where
{
val l1f0 =
xcomp01_h0exp_val(env0, h0f0)
val l1vs =
xcomp01_h0explst_arg(env0, npf1, h0es)
}
end (*let*) // end of [auxset_dapp]

(* ****** ****** *)

fun
auxset_ift1
( env0:
! compenv
, h0e0: h0exp
, tres: l1tmp): void =
let
//
val
loc0 = h0e0.loc()
val-
H0Eift1
( h0e1
, h0e2
, opt3) = h0e0.node()
//
val l1v1 =
xcomp01_l1valize
  (env0, l1v1) where
{
val l1v1 =
xcomp01_h0exp_val(env0, h0e1)
}
//
val blk2 =
xcomp01_h0exp_blk(env0, h0e2, tres)
val blk3 =
xcomp01_h0expopt_blk(env0, opt3, tres)
//
in
  let
  val
  lcmd =
  l1cmd_make_node
  ( loc0
  , L1CMDift1(l1v1, blk2, blk3))
  in
    xcomp01_lcmdadd_lcmd(env0, lcmd)
  end
end (*let*) // end of [ auxset_ift1 ]

(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

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
| H0Ekvar _ =>
  auxval_kvar(env0, h0e0)
//
| H0Eift1 _ =>
(
  l1val_tmp(tres)
) where
{
val
tres =
xltmpnew_tmp0(env0, loc0)
val () =
auxset_ift1(env0, h0e0, tres)
} (* end of [ H0Eift1 ] *)
//
| _ (* rest-of-h0exp *) =>
(
l1val_make_node(loc0, L1VALnone1(h0e0))
)
//
end // end of [xcomp01_h0exp_val]

(* ****** ****** *)

implement
xcomp01_h0exp_set
  (env0, h0e0, tres) =
let
val loc0 = h0e0.loc()
in(*in-of-let*)
//
case+
h0e0.node() of
//
|
H0Edapp _ =>
auxset_dapp(env0, h0e0, tres)
//
|
H0Eift1 _ =>
(
  auxset_ift1(env0, h0e0, tres)
)
//
|
_ (*rest-of-h0exp*) =>
let
val
l1v0 =
xcomp01_h0exp_val(env0, h0e0)
in
let
val
cmd0 =
l1cmd_make_node
( loc0, L1CMDmov(tres, l1v0) )
in
xcomp01_lcmdadd_lcmd(env0, cmd0)
end
end // end of [rest-of-h0exp]
//
end // end of [xcomp01_h0exp_set]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
implement
xcomp01_h0explst_val
  (env0, h0es) =
(
case+ h0es of
|
list_nil() =>
list_nil()
|
list_cons(h0e1, h0es) =>
let
val l1v1 =
xcomp01_h0exp_val(env0, h0e1)
in
list_cons(l1v1, l1vs) where
{
val l1vs =
xcomp01_h0explst_val(env0, h0es)
}
end
) (* end of [xcomp01_h0explst_val] *)
(* ****** ****** *)
implement
xcomp01_h0explst_arg
  (env0, npf1, h0es) =
(
case+ h0es of
|
list_nil() =>
list_nil()
|
list_cons(h0e1, h0es) =>
if
npf1 <= 0
then
let
val l1v1 =
xcomp01_h0exp_val(env0, h0e1)
in
  list_cons(l1v1, l1vs) where
  {
  val l1vs =
  xcomp01_h0explst_val(env0, h0es)
  }
end // end of [then]
else
let
val npf1 = npf1 - 1
in
xcomp01_h0explst_arg(env0, npf1, h0es)
end // end of [else]
) (* end of [xcomp01_h0explst_arg] *)
(* ****** ****** *)
//
implement
xcomp01_h0exp_blk
  (env0, h0e0, tres) =
(
xcomp01_lcmdpop0_blk(env0)
) where
{
  val () =
  xcomp01_lcmdpush_nil(env0)
  val () =
  xcomp01_h0exp_set(env0, h0e0, tres)
} (* end of [xcomp01_h0exp_blk] *)
//
(* ****** ****** *)
//
implement
xcomp01_h0expopt_blk
  (env0, opt0, tres) =
(
case+ opt0 of
| None() => l1blk_none()
| Some(h0e0) =>
  xcomp01_h0exp_blk(env0, h0e0, tres)
)
//
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
val () =
xcomp01_h0pat_ck01(env0, pat, l1v1)
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
