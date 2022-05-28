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
#staload $INTREP0(*open-pkg*)
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
#staload "./../SATS/tcomp01.sats"
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
//
implement
fprint_val<htcst> = fprint_htcst
implement
fprint_val<htvar> = fprint_htvar
implement
fprint_val<h0typ> = fprint_h0typ
//
(* ****** ****** *)
//
implement
fprint_val<h0con> = fprint_h0con
implement
fprint_val<h0cst> = fprint_h0cst
//
(* ****** ****** *)
fun
l1tmp_set_type
( tmp1: l1tmp
, h0t2: h0typ): void =
(
l1tmp_set_ltnm(tmp1, l1t2)
) where
{
  val l1t2 = tcomp01_h0typ(h0t2)
} (*where*)//end-of-[l1tmp_set_type]
(* ****** ****** *)

implement
xcomp01_hdcon
  (env0, hdc0) =
let
//
val
ctag = hdc0.ctag()
//
in//let
//
if
(ctag > 0)
then L1CONcon(hdc0)
else
let
//
val hdv0 =
h0con_get_dvar(hdc0)
val opt =
xcomp01_dvarfind(env0, hdv0)
//
(*
val () =
println!
("auxval_fcon: hdc0 = ", hdc0)
val () =
println!
("auxval_fcon: hdv0 = ", hdv0)
*)
//
in
case- opt of
~Some_vt(l1v0) => L1CONval(l1v0)
end // end of [else]
//
end(*let*)//end of [xcomp01_hdcon]

(* ****** ****** *)

local

(* ****** ****** *)

fun
auxdap1
( env0:
! compenv
, h0p0
: h0pat
, l1v1
: l1val ) : l1pck =
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
( env0:
! compenv
, h0p0
: h0pat
, l1v1
: l1val ) : l1pck =
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
auxnps(env0, npf1, h0ps)
end
else
auxlst(env0, h0ps, 0(*idx0*))
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
aux_trcd1
( env0:
! compenv
, h0p0
: h0pat
, l1v1
: l1val ) : l1pck =
let
val-
H0Ptrcd1
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
list_cons
(pck1, pcks) where
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
end (* end of [aux_trcd1] *)

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
val l1c1 =
xcomp01_hdcon(env0, hdc1)
in
  L1PCKcon(l1c1, l1v1(*ctag*))
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
H0Ptrcd1 _ =>
(
aux_trcd1
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
//
val-
H0Pvar(hdv0) = h0p0.node()
//
(*
val () =
let
val h0t0 = hdv0.type()
in
println!("auxvar: hdv0 = ", hdv0);
println!("auxvar: h0t0 = ", h0t0);
end(*let*) // end of [val]
*)
//
} (*where*) // end of [auxvar]

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
aux_trcd1
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
H0Ptrcd1
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
end (*let*) // end of [aux_trcd1]

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
| H0Ptrcd1 _ =>
  aux_trcd1(env0, h0p0, l1v1)
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

fun
auxh0g0
( env0:
! compenv
, h0g0
: h0gua): l1pck =
(
case+
h0g0.node() of
|
H0GUAexp
(h0e1) =>
let
val () =
xcomp01_lcmdpush_nil(env0)
val l1v1 =
xcomp01_h0exp_val(env0, h0e1)
in
L1PCKgexp(l1v1, blk1) where
{
  val
  blk1 =
  xcomp01_lcmdpop0_blk(env0)
}
end // end of [H0GUAexp]
|
H0GUAmat
( h0e1
, h0p2) =>
L1PCKgmat(h0e1, h0p2)
)
and
auxh0gs
( env0:
! compenv
, h0gs
: h0gualst): l1pcklst =
(
case+ h0gs of
|
list_nil() =>
list_nil()
|
list_cons
(h0g1, h0gs) =>
(
list_cons(pck1, pcks)
) where
{
val pck1 = auxh0g0(env0, h0g1)
val pcks = auxh0gs(env0, h0gs)
}
)

in(*in-of-local*)

implement
xcomp01_h0gpat_ck0
( env0
, hgp0, l1v1) =
(
case-
hgp0.node() of
|
H0GPATpat(h0p1) =>
xcomp01_h0pat_ck0
( env0, h0p1, l1v1 )
|
H0GPATgua(h0p1, h0gs) =>
let
//
val
pck1 =
xcomp01_h0pat_ck0
( env0, h0p1, l1v1 )
//
val
pcks =
auxh0gs( env0, h0gs )
//
in
  L1PCKgpat(pck1, pcks)
end
) where
{
(*
val () =
println! (
"xcomp01_h0gpat_ck0: hgp0 = ", hgp0
) (* println! *)
*)
} (* end of [xcomp01_h0gpat_ck0] *)

end // end of [local]

(* ****** ****** *)

implement
xcomp01_h0gpat_ck1
( env0
, hgp0, l1v1) =
(
case-
hgp0.node() of
|
H0GPATpat(h0p1) =>
xcomp01_h0pat_ck1
( env0, h0p1, l1v1 )
|
H0GPATgua(h0p1, h0gs) =>
let
  val () = 
  xcomp01_h0pat_ck1
  ( env0, h0p1, l1v1 )
in
(*
  xcomp01_h0gualst_ck1(h0gs)
*)
end
) (* end of [xcomp01_h0gpat_ck1] *)

(* ****** ****** *)

local

fun
auxnps
( env0:
! compenv
, arg0:
& int >> _
, npf0: int
, h0ps
: h0patlst): l1tmplst =
(
case+
h0ps of
|
list_nil() =>
list_nil()
|
list_cons
(h0p1, h0ps) =>
(
if
npf0 > 0
then
let
val npf0 = npf0 - 1
in
auxnps
( env0
, arg0, npf0, h0ps)
end // end of [then]
else let
//
val
loc1 = h0p1.loc()
val
h0t1 = h0p1.type()
//
val () =
(arg0 := arg0 + 1)
//
val
tmp1 =
xltmpnew_arg1
(env0, loc1, arg0(*idx*))
val () =
l1tmp_set_type(tmp1, h0t1)
//
val () =
println!
("auxnps: tmp1 = ", tmp1)
val () =
println!
("auxnps: h0t1 = ", h0t1)
//
in
list_cons(tmp1, tmps) where
{
val tmps =
auxnps(env0, arg0, npf0, h0ps)
}
end // end of [else]
)
) (* case *) // end of [auxnps]

(* ****** ****** *)

fun
auxlst
( env0:
! compenv
, arg0:
& int >> _
, hfgs
: h0faglst ): l1faglst =
(
case+ hfgs of
|
list_nil() => 
list_nil()
|
list_cons
(hfg1, hfgs) =>
(
case+
hfg1.node() of
|
H0FAGnpats
(npf0, h0ps) =>
let
//
val
lfg1 = 
auxnps
( env0
, arg0, npf0, h0ps )
val
lfgs =
auxlst(env0, arg0, hfgs)
//
in
  list_cons( lfg1, lfgs )
end
//
| H0FAGnone0(   ) =>
  auxlst(env0, arg0, hfgs)
| H0FAGnone1( _ ) =>
  auxlst(env0, arg0, hfgs)
)
) (*case*) // end of [auxlst]

in (* in-of-local *)

implement
xcomp01_h0faglst
 ( env0, hfgs ) =
let
var arg0: int = 0
in
  auxlst(env0, arg0, hfgs)
end // end of [xcomp01_h0faglst]

end // end of [local]

(* ****** ****** *)

local

(* ****** ****** *)
//
fun
auxpat_ck01
( env0:
! compenv
, arg0: int
, h0p1: h0pat
, tmp1: l1tmp): void =
let
//
val l1v1 = l1val_tmp(tmp1)
//
in
xcomp01_h0pat_ck01(env0, h0p1, l1v1)
end // end of [auxpat_ck01]
//
(* ****** ****** *)
//
fun
auxnps_ck01
( env0:
! compenv
, arg0:
& int >> _
, npf0: int
, h0ps
: h0patlst
, tmps
: l1tmplst): void =
(
case+
h0ps of
|
list_nil() => ()
|
list_cons(h0p1, h0ps) =>
(
if
npf0 > 0
then
let
val npf0 = npf0 - 1
in
auxnps_ck01
( env0
, arg0, npf0, h0ps, tmps)
end // end of [then]
else let
//
val () =
(arg0 := arg0 + 1)
//
val () =
auxpat_ck01
( env0, arg0, h0p1, tmp1 )
//
in
auxnps_ck01
( env0
, arg0, npf0, h0ps, tmps )
end // end of [else]
) where
{
  val-
  list_cons(tmp1, tmps) = tmps
}
) (*case*) // end of [auxnps_ck01]
//
(* ****** ****** *)
//
fun
auxhfg_ck01
( env0:
! compenv
, arg0:
& int >> _
, hfg0: h0fag
, lfg0: l1fag): void =
(
case+
hfg0.node() of
//
|
H0FAGnpats
(npf0, h0ps) =>
auxnps_ck01
( env0
, arg0, npf0, h0ps, lfg0
) (* H0FAGnpats *)
//
| H0FAGnone0( ) => ((*void*))
| H0FAGnone1(_) => ((*void*))
)
and
auxlst_ck01
( env0:
! compenv
, arg0:
& int >> _
, hfgs: h0faglst
, lfgs: l1faglst): void =
(
case+ hfgs of
|
list_nil() => ()
|
list_cons(x1, xs) =>
let
val-
list_cons(y1, ys) = lfgs
val () =
auxhfg_ck01(env0, arg0, x1, y1)
in
auxlst_ck01(env0, arg0, xs, ys)
end // list_cons
) (*case*) // end of [auxlst_ck01]
//
(* ****** ****** *)

in(*in-of-local*)

implement
xcomp01_h0faglst_ck01
(env0, hfgs, lfgs) =
(
xcomp01_lcmdpop0_blk(env0)
) where
{
//
val () =
xcomp01_lcmdpush_nil(env0)
//
var
arg0: int = 0 (*arg-index*)
//
val () =
auxlst_ck01(env0, arg0, hfgs, lfgs)
//
} // end of [xcomp01_h0faglst_ck01]

end // end of [local]

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
opt0 =
xcomp01_dvarfind(env0, x0)
//
in // let
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
auxval_fcst
( env0:
! compenv
, h0e0: h0exp): l1val =
let
//
val 
loc0 = h0e0.loc()
val-
H0Efcst(hdc) = h0e0.node()
//
in
//
if
h0cst_fcastq(hdc)
then
let
val
nam = "XATS2CC_fcast"
in
l1val_make_node(loc0, L1VALnam(nam))
end
else
l1val_make_node(loc0, L1VALcfun(hdc))
//
end // end of [auxval_fcst]

(* ****** ****** *)

fun
auxval_timp
( env0:
! compenv
, h0e0: h0exp): l1val =
let
val
loc0 = h0e0.loc()
val-
H0Etimp
( stmp
, h0e1, targ
, hdcl, tsub) = h0e0.node()
//
val-
H0Etcst
( hdc1, htia) = h0e1.node()
//
val
l1c1 =
l1cst_new_hdc( loc0, hdc1 )
val
ldcl =
xcomp01_h0dcl_timp(env0, l1c1, hdcl)
//
val () =
(
xcomp01_lcmdadd_lcmd(env0, lcmd)
) where
{
val
lcmd =
l1cmd_make_node(loc0, L1CMDdcl(ldcl))
}
//
in
l1val_make_node(loc0, L1VALctmp(l1c1, ldcl))
end // end of [auxval_timp]

(* ****** ****** *)

local

fun
auxlst_h0dcl
( env0:
! compenv
, dcls: h0dclist): void =
(
case+ dcls of
|
list_nil() => ()
|
list_cons
(hdcl, dcls) =>
(
auxlst_h0dcl(env0, dcls)
) where
{
val
ldcl =
xcomp01_h0dcl_dcl(env0, hdcl)
val
lcmd =
l1cmd_make_node
( hdcl.loc(), L1CMDdcl(ldcl) )
val () =
xcomp01_lcmdadd_lcmd(env0, lcmd)
}
) (* end of [auxlst_h0dcl] *)

in(*in-of-local*)

fun
auxval_let
( env0:
! compenv
, h0e0: h0exp): l1val =
let
//
val-
H0Elet
(dcls, h0e1) = h0e0.node()
//
val () =
auxlst_h0dcl( env0, dcls )
//
in
xcomp01_h0exp_val(env0, h0e1)
end // end of [auxval_let]

end // end of [local]

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

local
//
fun
auxpck0
( env0:
! compenv
, l1v1
: l1val
, hcl1
: h0clau): l1pck =
(
case-
hcl1.node() of
| H0CLAUpat
  (hgp1) =>
  xcomp01_h0gpat_ck0
  ( env0, hgp1, l1v1 )
| H0CLAUexp
  (hgp1, h0e1) =>
  xcomp01_h0gpat_ck0
  ( env0, hgp1, l1v1 )
)
and
auxpck0lst
( env0:
! compenv
, l1v1
: l1val
, hcls
: h0claulst): l1pcklst =
(
case+ hcls of
|
list_nil() => list_nil()
|
list_cons(hcl1, hcls) =>
list_cons(pck1, pcks) where
{
val pck1 =
auxpck0(env0, l1v1, hcl1)
val pcks =
auxpck0lst(env0, l1v1, hcls)
}
) (* end of [auxpck0lst] *)
//
fun
auxpck1
( env0:
! compenv
, l1v1
: l1val
, hcl1
: h0clau
, tres: l1tmp): l1blk =
(
case-
hcl1.node() of
|
H0CLAUexp
(h0gp, h0e1) =>
let
val () =
xcomp01_lcmdpush_nil(env0)
//
val () =
xcomp01_h0exp_set
( env0, h0e1, tres ) where
{
val () =
xcomp01_h0gpat_ck1(env0, h0gp, l1v1)
}
//
in
  xcomp01_lcmdpop0_blk(env0)
end // end of [H0CLAUexp]
)
//
and
auxpck1lst
( env0:
! compenv
, l1v1
: l1val
, hcls
: h0claulst
, tres: l1tmp): l1blklst =
(
case+ hcls of
|
list_nil() => list_nil()
|
list_cons(hcl1, hcls) =>
list_cons
( auxpck1
  (env0, l1v1, hcl1, tres)
, auxpck1lst
  (env0, l1v1, hcls, tres))
)
//
in(*in-of-local*)

fun
auxset_case
( env0:
! compenv
, h0e0: h0exp
, tres: l1tmp): void =
let
//
val
loc0 = h0e0.loc()
val-
H0Ecase
( knd0
, h0e1
, hcls) = h0e0.node()
//
val
l1v1 =
xcomp01_l1valize
  ( env0, l1v1 ) where
{
val
l1v1 =
xcomp01_h0exp_val(env0, h0e1)
}
//
(*
// HX: [tcas] is an int
*)
local
val
ltnm = l1tnm_none0()
val
lctp =
L1CTPname
("xcmp_tcas_t")(*sint*)
val () = ltnm.lctp(lctp)
in(* in-of-local *)
val
tcas =
xltmpnew_tmp0(env0, loc0)
val () =
l1tmp_set_ltnm(tcas, ltnm)
end // end of [local]
//
val
pcks =
auxpck0lst(env0, l1v1, hcls)
val
blks =
auxpck1lst(env0, l1v1, hcls, tres)
//
in
let
val
lcmd =
l1cmd_make_node
( loc0,
  L1CMDcase
  ( knd0
  , l1v1, tcas, pcks, blks))
in
  xcomp01_lcmdadd_lcmd(env0, lcmd)
end (* end-of-let *) end (* auxset_case *)

end // end of [local]

(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

implement
xcomp01_h0exp_val
  (env0, h0e0) =
let
//
val
loc0 = h0e0.loc()
val
h0t0 = h0e0.type()
//
(*
val () =
println!
("xcomp01_h0exp_val: h0e0 = ", h0e0)
*)
//
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
| H0Eflt(tok) =>
  l1val_make_node
  (loc0, L1VALflt(tok))
| H0Estr(tok) =>
  l1val_make_node
  (loc0, L1VALstr(tok))
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
| H0Efcst _ =>
  auxval_fcst(env0, h0e0)
//
| H0Etimp _ =>
  auxval_timp(env0, h0e0)
//
|
H0Edapp _ =>
let
val () =
auxset_dapp
(env0, h0e0, tres)
in
  l1val_tmp( tres )
end where
{
//
val
tres =
xltmpnew_tmp0(env0, loc0)
val () =
l1tmp_set_type(tres, h0t0)
//
val () =
println!
("xcomp01_h0exp_val: tres = ", tres)
val () =
println!
("xcomp01_h0exp_val: h0t0 = ", h0t0)
//
} (* end of [H0Edapp] *)
//
| H0Elet _ =>
(
auxval_let( env0, h0e0 )
)
//
| H0Eift1 _ =>
let
val () =
auxset_ift1
(env0, h0e0, tres)
in
l1val_tmp(tres) end where
{
//
val
tres =
xltmpnew_tmp0(env0, loc0)
val () =
l1tmp_set_type(tres, h0t0)
//
val () =
println!
("xcomp01_h0exp_val: tres = ", tres)
val () =
println!
("xcomp01_h0exp_val: h0t0 = ", h0t0)
//
} (* end of [ H0Eift1 ] *)
//
|
H0Ecase _ =>
(
  l1val_tmp(tres)
) where
{
//
val
tres =
xltmpnew_tmp0(env0, loc0)
val () =
l1tmp_set_type(tres, h0t0)
//
val () =
println!
("xcomp01_h0exp_val: tres = ", tres)
val () =
println!
("xcomp01_h0exp_val: h0t0 = ", h0t0)
//
val () =
auxset_case(env0, h0e0, tres)
} (* end of [H0Ecase] *)
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
//
val
loc0 = h0e0.loc()
(*
val
h0t0 = h0e0.type()
*)
//
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
L1PKG(ltmps, ldcls)
) where
{
//
(*
val () =
xcomp01_initize( )
*)
//
val
env0 =
compenv_make_nil( )
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
local
(*
HX-2022-04-25:
[tcomp01_h0dclist]
populates [the_ltnmmap]!
*)
val () =
  tcomp01_h0dclist(hdcls)
in(*in-of-local*)
val () =
the_ltnmmap_ctpize((*void*))
end // end of [local]
//
val () =
tcomp01_the_ltnmmap((*void*))
//
val
ldcls =
xcomp01_h0dclist_dcl(env0, hdcls)
//
val ltmps = compenv_free_top(env0)
//
}(*case*)//end of [xcomp01_package]

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
H0Cfundclst
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
( env0:
! compenv
, hfds
: h0fundeclist): void =
(
case+ hfds of
|
list_nil() => ()
|
list_cons
(hfd1, hfds) =>
let
  val+
  H0FUNDECL
  ( rcd ) = hfd1
  val loc = rcd.loc
  val nam = rcd.nam
  val hdc = rcd.hdc
  val
  itm =
  l1val_make_node
  (loc, L1VALcfun(hdc))
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
xcomp01_h0fundeclist(env0, hfds)
//
val () = xcomp01_dvarpop_fun0(env0)
//
in
l1dcl_make_node
(loc0, L1DCLfundclst(knd0, mopt, lfds))
//
end where
{
//
val
loc0 = dcl0.loc()
val-
H0Cfundclst
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
aux_valdclst
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
val
loc0 = dcl0.loc()
val-
H0Cvaldclst
( knd0
, mopt, hvds) = dcl0.node()
val
lvds =
xcomp01_h0valdeclist(env0, hvds)
in
l1dcl_make_node
(loc0, L1DCLvaldclst(knd0, mopt, lvds))
end // end of [aux_valdclst]

(* ****** ****** *)

fun
aux_vardclst
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
val
loc0 = dcl0.loc()
val-
H0Cvardclst
( knd0
, mopt, hvds) = dcl0.node()
val
lvds =
xcomp01_h0vardeclist(env0, hvds)
in
l1dcl_make_node
(loc0, L1DCLvardclst(knd0, mopt, lvds))
end // end of [aux_vardclst]

(* ****** ****** *)

fun
aux_datatype
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
//
val
loc0 = dcl0.loc()
//
val-
H0Cdatatype
(htcs) = dcl0.node((*void*))
//
(*
val-
list_cons(htc1, _) = htcs
val-
Some(hdcs) = htc1.hdconlst()
val () =
println!
("aux_datatype: htc1 = ", htc1)
val () =
println!
("aux_datatype: hdcs = ", hdcs)
*)
//
in
l1dcl_make_node(loc0, L1DCLdatatype(htcs))
end // end of [aux_datatype]

(* ****** ****** *)

fun
aux_impdecl3
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
//
val-
H0Cimplmnt3
( tok0
, stmp, mopt
, htqa
, hdc1, htia
, hfgs, body) = dcl0.node()
//
in
case+ htia of
|
HTIARGnone _ =>
aux_impdecl3_none(env0, dcl0)
|
HTIARGsome _ => // function-template
aux_impdecl3_some(env0, dcl0)
end // end of [aux_impdecl3]
//
and
aux_impdecl3_none
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
//
val-
H0Cimplmnt3
( knd0
, stmp, mopt
, htqa
, hdc1, htia
, hfgs, body) = dcl0.node()
//
var rval
  : l1valopt = None()
//
val () =
xcomp01_flevinc(env0)
val () =
xcomp01_dvaradd_fun0(env0)
val () =
xcomp01_ltmpadd_fun0(env0)
//
val
flev =
xcomp01_flevget(env0)
//
val
lfgs =
xcomp01_h0faglst(env0, hfgs)
val
blk0 =
xcomp01_h0faglst_ck01(env0, hfgs, lfgs)
//
val
blk1 =
let
val ( ) =
xcomp01_lcmdpush_nil( env0 )
//
val
l1v1 =
xcomp01_h0exp_val(env0, body)
val ( ) = (rval := Some(l1v1))
//
in
  xcomp01_lcmdpop0_blk( env0 )
end // end of [Some]
//
in
let
//
  val () =
  xcomp01_flevdec(env0)
  val () =
  xcomp01_dvarpop_fun0( env0 )
  val tmps =
  xcomp01_ltmppop_fun0( env0 )
//
(*
val ( ) =
println!
("xcomp01_impdecl3: tmps = ", tmps)
*)
//
val
loc0 = dcl0.loc()
//
val
limp =
L1IMPLMNT3@{
  loc=loc0
, hdc=hdc1
, hfg=hfgs
, lfg=lfgs
, def=rval
, lev=flev
, lts=tmps
, lfg_blk=blk0, def_blk=blk1
} (* L1IMPLMNT3 *)
//
in
l1dcl_make_node
( loc0
, L1DCLimplmnt3(knd0, mopt, limp))
end
//
end // end of [aux_impdecl3_none]
//
and
aux_impdecl3_some
( env0:
! compenv
, dcl0: h0dcl): l1dcl =
let
val loc0 = dcl0.loc()
in
l1dcl_make_node(loc0, L1DCLnone0(*none*))
end // end of [aux_impdecl3_some]

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
val () =
println!
("xcomp01_h0dcl_dcl: dcl0 = ", dcl0)
*)
in(*in-of-let*)
//
case+
dcl0.node() of
//
|
H0Cfundclst _ =>
aux_fundecl(env0, dcl0)
//
|
H0Cvaldclst _ =>
aux_valdclst(env0, dcl0)
|
H0Cvardclst _ =>
aux_vardclst(env0, dcl0)
//
|
H0Cdatatype _ =>
aux_datatype(env0, dcl0)
//
|
H0Cimplmnt3 _ =>
aux_impdecl3(env0, dcl0)
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

implement
xcomp01_h0dcl_timp
(env0, l1c1, hdcl) =
let
val
loc0=hdcl.loc()
in // in-of-let
//
case+
hdcl.node() of
//
|
H0Cfundclst _ =>
let
val
ldcl =
aux_fundecl_fun(env0, hdcl)
in
  l1dcl_make_node
  (loc0, L1DCLtimpcst3(l1c1, ldcl))
end // end of [H0Cfundclst]
//
|
H0Cimplmnt3 _ =>
let
val
ldcl =
aux_impdecl3_none(env0, hdcl)
in
  l1dcl_make_node
  (loc0, L1DCLtimpcst3(l1c1, ldcl))
end // end of [H0Cimpdecl3]
//
| _ (* else *) =>
(
  l1dcl_make_node
  (loc0, L1DCLtimpcst3(l1c1, ldcl))
) where
{
val
ldcl =
l1dcl_make_node(loc0, L1DCLnone1(hdcl))
}
//
end (*let*) // end of [xcomp01_h0dcl_timp]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
//
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
//
(* ****** ****** *)

implement
xcomp01_h0fundecl
  (env0, dcl0) =
let
//
val+
H0FUNDECL
( rcd ) = dcl0
//
val loc = rcd.loc
val nam = rcd.nam
val hdc = rcd.hdc
val hfg = rcd.hfg
//
var def
  : l1valopt = None()
var rtp
  : l1tnmopt = None()
//
val () =
xcomp01_flevinc(env0)
val () =
xcomp01_dvaradd_fun0(env0)
val () =
xcomp01_ltmpadd_fun0(env0)
//
(*
//
(*
HX:
This needs to be done
earlier due to recursion
*)
//
local
  val
  itm =
  l1val_make_node
  (loc, L1VALfcst(hdc))
in
  val () =
  xcomp01_dvaradd_bind
  (env0, nam, itm(*l1val*))
end // end of [local]
//
*)
//
val
flev =
xcomp01_flevget(env0)
//
val
lfg =
(
case+ hfg of
|
None() =>
None()
|
Some(hfgs) =>
Some
(xcomp01_h0faglst(env0, hfgs))
) : l1faglstopt
//
local
val
l1t1 =
tcomp01_h0typ(rcd.rtp)
in
val () = ( rtp := Some(l1t1) )
end // end of [local]
//
val
blk0 =
(
case+ hfg of
|
None() =>
l1blk_none()
|
Some(hfgs) =>
let
val-
Some(lfgs) = lfg
in
xcomp01_h0faglst_ck01(env0, hfgs, lfgs)
end
) : l1blk // end-of-val
//
val
blk1 =
(
case+ rcd.def of
|
None() => l1blk_none()
|
Some(h0e1) =>
let
//
val ( ) =
xcomp01_lcmdpush_nil(env0)
//
val
l1v1 =
xcomp01_h0exp_val(env0, h0e1)
val ( ) = (def := Some(l1v1))
//
in
  xcomp01_lcmdpop0_blk( env0 )
end // end of [Some]
) : l1blk // end of [val]
//
in
let
//
  val () =
  xcomp01_flevdec(env0)
  val () =
  xcomp01_dvarpop_fun0(env0)
  val flts =
  xcomp01_ltmppop_fun0(env0)
//
(*
val ( ) =
println!
("xcomp01_h0fundecl: lts = ", lts)
*)
//
in
  L1FUNDECL@{
    loc=loc
  , nam=nam, hdc=hdc
  , hfg=hfg, lfg=lfg
  , def=def, rtp=rtp
  , lev=flev
  , lts=flts
  , lfg_blk=blk0, def_blk=blk1
} (* L1FUNDECL *)
end
end // end of [xcomp01_h0fundecl]

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
xcomp01_h0fundeclist
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
val x0 = xcomp01_h0fundecl(env0, x0)
val xs = xcomp01_h0fundeclist(env0, xs)
}
) (* end of [xcomp01_h0fundeclist] *)
//
end // end of [local]

(* ****** ****** *)

implement
xcomp01_h0valdecl
  (env0, dcl0) =
let
//
val+
H0VALDECL
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
  L1VALDECL@{
    loc=loc
  , pat=pat, def=res, def_blk=blk
  } (* LVALDECL *)
end
(*let*) // end of [xcomp01_h0valdecl]

(* ****** ****** *)

implement
xcomp01_h0valdeclist
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
val x0 = xcomp01_h0valdecl(env0, x0)
val xs = xcomp01_h0valdeclist(env0, xs)
}
) (*case*)//end of [xcomp01_h0valdeclist]

(* ****** ****** *)

implement
xcomp01_h0vardecl
(env0, dcl0) =
let
//
val+
H0VARDECL
( rcd ) = dcl0
//
val loc = rcd.loc
//
in
  L1VARDECL@{ loc=loc }
end (*let*) // end of [xcomp01_h0vardecl]

(* ****** ****** *)

implement
xcomp01_h0vardeclist
  (env0, xs) =
(
case+ xs of
|
list_nil() => list_nil()
|
list_cons(x0, xs) =>
list_cons(x0, xs) where
{
val x0 = xcomp01_h0vardecl(env0, x0)
val xs = xcomp01_h0vardeclist(env0, xs)
}
)(*case*) // end of [xcomp01_h0vardeclist]

(* ****** ****** *)

(* end of [xats_xcomp01_dynexp.dats] *)
