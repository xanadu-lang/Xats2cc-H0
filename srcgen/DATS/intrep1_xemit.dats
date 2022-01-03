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
#staload $LEX(* open *)
(* ****** ****** *)
#staload $S1E(* open *)
#staload $S2E(* open *)
(* ****** ****** *)
#staload $INTREP0(* open *)
(* ****** ****** *)
overload
fprint with $STM.fprint_stamp
overload
fprint with $SYM.fprint_symbol
overload
fprint with $LOC.fprint_location
(* ****** ****** *)
overload
fprint
with $FP0.fprint_filpath_full2
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
(* ****** ****** *)

implement
xemit01_txt00
(out, txt) =
(
fprint(out, txt)
) (* end of [xemit01_txt00] *)
implement
xemit01_txtln
(out, txt) =
(
fprint!(out, txt, '\n')
) (* end of [xemit01_txtln] *)

(* ****** ****** *)
implement
xemit01_newln
( out ) =
(
  fprint_char(out, '\n')
) (* end of [xemit01_newln] *)
(* ****** ****** *)

implement
xemit01_hdvar
(out, hdv) =
fprint(out, hdv.sym())
(* ****** ****** *)
implement
xemit01_hdcon
(out, hdc) =
let
val
tag = hdc.tag()
in
if
tag >= 0
then
fprint(out, tag)
else
fprint(out, hdc.sym())
end // end of [xemit01_hdcon]

(* ****** ****** *)

local

(* ****** ****** *)

fun
auxnone
( out
: FILEref
, hdc: hdcst): void =
{
//
val
sym = hdc.sym()
val
loc = hdc.loc()
//
val () =
fprint(out, sym)
//
val () =
let
val
ntot = loc.beg_ntot()
in
fprint!(out, "_", ntot, "_")
end // end of [let]
//
} (* end of [auxnone] *)

(* ****** ****** *)

fun
auxsome
( out
: FILEref
, gnm: g1nam
, hdc: hdcst): void =
(
case+ gnm of
| G1Nnil() =>
  auxsome_nil(out, hdc, gnm)
| G1Nid0(id0) =>
  auxsome_id0(out, hdc, gnm)
| G1Nnone0() =>
  auxsome_none(out, hdc, gnm)
| _(*rest-of-g1nam*) =>
  auxsome_rest(out, hdc, gnm)
)

and
auxsome_nil
( out
: FILEref
, hdc: hdcst
, gnm: g1nam): void =
let
val
sym = hdc.sym()
in
  fprint!(out, sym)
end // end of [auxsome_nil]

and
auxsome_id0
( out
: FILEref
, hdc: hdcst
, gnm: g1nam): void =
let
val-
G1Nid0(sym) = gnm
in
  fprint!(out, sym)
end // end of [auxsome_nil]

and
auxsome_none
( out
: FILEref
, hdc: hdcst
, gnm: g1nam): void =
(
  auxsome_nil(out, hdc, gnm)
)

and
auxsome_rest
( out
: FILEref
, hdc: hdcst
, gnm: g1nam): void =
let
val
sym = hdc.sym()
in
  fprint!
  (out, sym, "_**EXNAME**_")
end // end of [auxsome_rest]

(* ****** ****** *)

in(*in-of-local*)
//
implement
xemit01_hdcst
(out, hdc) =
let
val
xnm = hdc.xnam()
in
//
case+ xnm of
|
X2NAMnone() =>
(
  auxnone(out, hdc)
)
|
X2NAMsome(gnm) =>
(
  auxsome(out, gnm, hdc)
)
//
end // end of [xemit01_hdcst]
//
end // end of [local]

(* ****** ****** *)

implement
xemit01_hfarg
( out
, lev
, hfg, i0) =
(
case+
hfg.node() of
|
HFARGnpats
(npf1, h0ps) => i1 where
{
val () = xemit01_txt00(out, "(")
val i1 = auxlst(npf1, i0, i0, h0ps)
val () = xemit01_txt00(out, ")")
}
|
HFARGnone0 _ => i0 where
{
val () = xemit01_txt00(out, "(")
val () = xemit01_txt00(out, ")")
}
|
HFARGnone1 _ => i0 // skipped
(*
let
val () = fprint(out, "(*ERROR*)") in i0
end
*)
) where
{
//
fun
auxlst
( npf1: int
, i0: int
, i1: int
, h0ps: h0patlst): int =
(
case+ h0ps of
|
list_nil() => i1
|
list_cons
(h0p1, h0ps) =>
if
npf1 > 0
then
let
val npf1 = npf1-1
in
auxlst(npf1, i0, i1, h0ps)
end
else
let
//
val () =
if
(i1 > i0)
then
xemit01_txt00(out, ", ")
//
val i1 = i1 + 1
//
val () =
fprint!
(out, "a", lev, "x", i1)
in
auxlst(npf1, i0, i1, h0ps)
end
) (* end of [auxlst] *)
//
} (* end of [xemit01_hfarg] *)

(* ****** ****** *)

implement
xemit01_hfarglst
( out
, lev
, hfgs, i0) =
(
case+ hfgs of
|
list_nil() => i0
|
list_cons(hfg1, hfgs) =>
let
val i1 =
xemit01_hfarg(out, lev, hfg1, i0)
in
xemit01_hfarglst(out, lev, hfgs, i1)
end
) (* end of [xemit01_hfarglst] *)

(* ****** ****** *)

implement
xemit01_ldcon
(out, ldc) =
(
case+ ldc of
| LDCONcon(hdc) =>
  xemit01_hdcon(out, hdc)
| LDCONval(l1v) =>
  xemit01_l1val(out, l1v)
) (* end of [xemit01_ldcon] *)

(* ****** ****** *)
//
implement
xemit01_l1tmp
(out, tmp0) =
(
  fprint_l1tmp(out, tmp0)
) (* end of [xemit01_l1tmp] *)
//
(* ****** ****** *)
implement
xemit01_l1val
(out, l1v0) =
(
case+
l1v0.node() of
//
| L1VALnone0() =>
{
  val () = fprint( out, "null" )
}
//
| _ (* else *) => fprint(out, l1v0)
//
) where
{
(*
val () =
fprintln!
(out, "xemit01_l1val: l1v0 = ", l1v0)
*)
} (*where*) // end of [xemit01_l1val]
(* ****** ****** *)
local
//
fun
aux_mov
( out
: FILEref
, lcmd
: l1cmd): void =
{
val () =
xemit01_l1tmp(out, tres)
val () =
xemit01_txt00(out, " = ")
val () =
xemit01_l1val(out, l1v1)
} where
{
val-
L1CMDmov
(tres, l1v1) = lcmd.node()
} (* where *) // end of [aux_mov]
//
fun
aux_con
( out
: FILEref
, lcmd
: l1cmd): void =
{
val () =
xemit01_l1tmp(out, tres)
val () =
xemit01_txt00(out, " = ")
//
val () =
xemit01_txt00(out, "[")
//
local
fun
loop
( n0: int
, xs
: l1valist): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  loop(n0+1, xs)
) where
{
val () =
if
(n0 > 0)
then
xemit01_txt00(out, ", ")
val () = xemit01_l1val(out, x0)
} (* list_cons *)
)
in (*in-of-local*)
//
val () =
let
val-
L1VALcon
( ldc0 ) = l1f0.node()
in
xemit01_ldcon(out, ldc0)
end
val () = loop( 1, l1vs )
val () = xemit01_txt00(out, "]")
//
end (* end of [local] *)
//
} where
{
//
val-
L1CMDapp
( tres
, l1f0, l1vs) = lcmd.node()
//
} (* where *) // end of [aux_con]
//
fun
aux_app
( out
: FILEref
, lcmd
: l1cmd): void =
{
val () =
xemit01_l1tmp(out, tres)
val () =
xemit01_txt00(out, " = ")
//
val () =
xemit01_l1val( out, l1f0 )
//
local
fun
loop
( n0: int
, xs
: l1valist): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  loop(n0+1, xs)
) where
{
//
val () =
if
(n0 > 0)
then
xemit01_txt00(out, ", ")
//
val () =
xemit01_l1val( out, x0 )
//
} (* list_cons *)
)
in(* in-of-local *)
//
val () =
xemit01_txt00(out, "(")
val () = loop( 0, l1vs )
val () = xemit01_txt00(out, ")")
//
end (* end of [local] *)
//
} where
{
//
val-
L1CMDapp
( tres
, l1f0, l1vs) = lcmd.node()
//
} (* where *) // end of [aux_app]
//
fun
aux_blk
( out
: FILEref
, lcmd
: l1cmd): void =
(
case+ blk1 of
|
L1BLKnone() => ()
|
L1BLKsome(xs) =>
{
val () =
xemit01_txtln( out, "{" )
val () =
xemit01_l1cmdlst(out, xs)
val () =
xemit01_txtln( out, "}" )
}
) where
{
val-
L1CMDblk(blk1) = lcmd.node()
} (* where *) // end of [aux_blk]
//
(* ****** ****** *)

fun
aux_ift1
( out
: FILEref
, lcmd
: l1cmd): void =
{
//
val() =
xemit01_txtln(out, "if")
val() = xemit01_txt00(out, "(")
val() = xemit01_l1val(out, l1v1)
val() = xemit01_txtln(out, ")")
//
val() =
fprint!(out, "// then\n")
val() = xemit01_txtln(out, "{")
val() = xemit01_l1blk(out, blk2)
val() = xemit01_txtln(out, "} // if-then")
//
val() =
xemit01_txtln(out, "else")
val() = xemit01_txtln(out, "{")
val() = xemit01_l1blk(out, blk3)
val() = xemit01_txtln(out, "} // if-else")
//
} where
{
val-
L1CMDift1
(l1v1, blk2, blk3) = lcmd.node()
} (* where *) // end of [aux_ift1]

(* ****** ****** *)

in(*in-of-local*)

implement
xemit01_l1cmd
(out, lcmd) =
(
case+
lcmd.node() of
|
L1CMDmov _ => aux_mov(out, lcmd)
//
|
L1CMDapp
(_, l1f0, _) =>
(
case+
l1f0.node() of
|
L1VALcon _ => aux_con(out, lcmd)
|
_ (*else*) => aux_app(out, lcmd)
)
//
|
L1CMDblk _ => aux_blk(out, lcmd)
//
|
L1CMDift1 _ => aux_ift1(out, lcmd)
//
|
_ (* else *) => fprint!(out, "//", lcmd)
//
) (*xemit01_l1cmd*) end // end of [local]
(* ****** ****** *)
implement
xemit01_l1cmdlst
  (out, cmds) =
(
  loop( cmds )
) where
{
fun
loop
( cmds
: l1cmdlst): void =
(
case+ cmds of
|
list_nil() => ()
|
list_cons
(x0, cmds) =>
loop(cmds) where
{
val()=
xemit01_l1cmd(out, x0)
val()=
xemit01_txtln(out, ";")
}
)
} (*end*) // xemit01_l1cmdlst
(* ****** ****** *)
implement
xemit01_l1blk
(out, blk0) =
(
case+ blk0 of
|
L1BLKnone() => ()
|
L1BLKsome(cmds) =>
{
  val() =
  xemit01_l1cmdlst(out, cmds)
}
) (* end of [xemit01_l1blk] *)
(* ****** ****** *)
implement
xemit01_fargdecs
( out
, narg, flev) =
(
  loop(0(*i0*))
) where
{
fun
loop(i0: int): void =
if
(i0 < narg)
then
(
  loop(i1) // end-of-then
) where
{
val i1 = i0+1
val () =
fprint!
( out
, "let", " "
, "a", flev, "y", i1, ";\n")
}
// else () // end-of-else
} (* end of [xemit01_fargdecs] *)
(* ****** ****** *)
implement
xemit01_ftmpdecs
( out, tmps ) =
(
case+ tmps of
|
list_nil() => ()
|
list_cons(t1, ts) =>
let
val i0 = t1.arg()
in
if
(i0 > 0)
then
(
xemit01_ftmpdecs(out, ts)
)
else
(
xemit01_ftmpdecs(out, ts)
) where
{
//
val () =
xemit01_txt00(out, "let ")
//
val () = xemit01_l1tmp(out, t1)
val () = xemit01_txtln(out, ";")
//
} (* end of [else] *)
end // end of [let]
) (* end of [xemit01_ftmpdecs] *)
(* ****** ****** *)
//
local

fun
aux_fundecl
( out
: FILEref
, dcl0: l1dcl): void =
let
val-
L1DCLfundecl
( knd0
, mopt
, lfds) = dcl0.node()
//
fun
isfnx
(knd0: token): bool =
(
case-
knd0.node() of
|
T_FUN(fnk) =>
(
case+ fnk of
| FNKfn2() => true
| FNKfnx() => true
| _(*else*) => false
)
) (* end of [isfnx] *)
//
in
ifcase
|
isfnx(knd0) =>
xemit01_l1dcl_fnx(out, dcl0)
|
_(* else *) =>
xemit01_l1dcl_fun(out, dcl0)
end // end of [aux_fundecl]

(* ****** ****** *)

fun
aux_valdecl
( out
: FILEref
, dcl0: l1dcl): void =
let
//
fun
auxlvd0
( lvd0
: lvaldecl): void =
{
//
val+
LVALDECL(rcd) = lvd0
//
val () =
xemit01_txtln(out, "{")
val () =
xemit01_l1blk(out, rcd.def_blk)
val () =
fprintln!
( out, "} // val(", rcd.pat, ")" )
//
} (* end of [auxlvd0] *)
//
(* ****** ****** *)
//
and
auxlvds
( lvds
: lvaldeclist): void =
(
case lvds of
|
list_nil() => ()
|
list_cons
(lvd0, lvds) =>
{
  val () = auxlvd0(lvd0)
  val () = auxlvds(lvds)
}
) (* end of [auxlvds] *)
//
in
let
val-
L1DCLvaldecl
( knd0
, mopt
, lvds) = dcl0.node() in auxlvds(lvds)
end
end // end of [aux_valdecl]

(* ****** ****** *)

in(*in-of-local*)

implement
xemit01_l1dcl
(out, dcl0) =
let
//
val
loc0 = dcl0.loc()
//
val () =
fprint!(out, "// ")
val () =
fprintln!(out, loc0)
//
(*
val () = fprint!(out, "// ")
val () = fprintln!(out, dcl0)
*)
in(*in-of-let*)
//
case+
dcl0.node() of
//
|
L1DCLfundecl _ =>
{
val()=aux_fundecl(out, dcl0)
}
//
|
L1DCLvaldecl _ =>
{
val()=aux_valdecl(out, dcl0)
}
//
(*
|
L1DCLvardecl _ =>
{
val()=aux_vardecl(out, dcl0)
}
*)
| _ (* else *) =>
{
val () = fprint!(out, "// ", dcl0)
}
//
end (*xemit01_l1dcl*) end // end-of-local
//
(* ****** ****** *)
//
implement
xemit01_package
  (out, lpkg) =
{
val () = auxtmps(tmps)
val () = auxdcls(dcls) 
} where
{
//
val+
L1PKG
(tmps, dcls) = lpkg
//
fun
auxtmps
( xs
: l1tmplst): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  auxtmps(xs)) where
{
val () = xemit01_l1tmp(out, x0)
val () = xemit01_txtln(out, ";")
} (* list_cons *)
) (* end of [auxtmps] *)
//
fun
auxdcls
( xs
: l1dclist): void =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
let
(*
val () = xindnt(0)
*)
val () =
xemit01_l1dcl(out, x0)
val () = xemit01_newln(out)
val () = xemit01_newln(out) in auxdcls(xs)
end // list_cons
) (* end of [auxdcls] *)
//
} (*where*) // end of [xemit01_package]
//
(* ****** ****** *)

implement
xemit01_l1dcl_fun
  (out, dcl0) =
let
//
fun
auxlfd0
( lfd0
: lfundecl): void =
let
//
val+
LFUNDECL(rcd) = lfd0
//
in
case+
rcd.def_blk of
|
L1BLKnone _ => ()
|
L1BLKsome _ => auxlfd0_some(lfd0)
end // end of [auxlfd0]
//
and
auxlfd0_some
( lfd0
: lfundecl): void =
let
//
val+
LFUNDECL(rcd) = lfd0
//
val () =
xemit01_txtln
(out, "function")
//
val () =
xemit01_hdcst(out, rcd.hdc)
val narg =
(
case+
rcd.hag of
|
None() => 0
|
Some
(rcd_hag) => narg where
{
val
narg =
xemit01_hfarglst
( out
, rcd.lev
, rcd_hag, 0(*base*) )
val () = xemit01_newln(out)
}
)
//
val () =
xemit01_txtln(out, "{")
//
val () =
xemit01_ftmpdecs(out, rcd.lts)
//
val () =
xemit01_l1blk(out, rcd.hag_blk)
val () =
xemit01_l1blk(out, rcd.def_blk)
//
val () =
(
case+
rcd.def of
|
None() => ()
|
Some(res) =>
{
//
val () =
xemit01_txt00(out, "return ")
val () = xemit01_l1val(out, res)
val () = xemit01_txt00(out, ";\n")
//
}
) : void // end-of-val
in
  fprintln!
  (out, "} // function // ", rcd.hdc)
end (* end of [auxlfd0_some] *)
//
(* ****** ****** *)
//
and
auxlfds
( lfds
: lfundeclist): void =
(
case lfds of
|
list_nil() => ()
|
list_cons
(lfd0, lfds) =>
{
  val () = auxlfd0(lfd0)
  val () = auxlfds(lfds)
}
)
//
in
//
let
val-
L1DCLfundecl
( knd0
, mopt
, lfds) = dcl0.node() in auxlfds(lfds)
end // end of [let]
//
end where
{
(*
  val () =
  println!
  ("xemit01_l1dcl_fun: dcl0 = ", dcl0)
*)
} (*where*) // end of [xemit01_l1dcl_fun]

(* ****** ****** *)
//
implement
xemit01_l1dcl_fnx
  (out, dcl0) =
(
  xemit01_l1dcl_fun(out, dcl0)
)
//
(* ****** ****** *)

(* end of [xats_intrep1_xemit.dats] *)
