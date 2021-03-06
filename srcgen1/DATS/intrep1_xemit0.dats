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
xemit01_int00
(out, int) =
(
  fprint(out, int)
) (* end of [xemit01_int00] *)
implement
xemit01_btf00
(out, btf) =
(
  fprint(out, btf)
) (* end of [xemit01_btf00] *)

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
xemit01_h0var
(out, hdv) =
fprint(out, hdv.sym())
(* ****** ****** *)
implement
xemit01_h0con
(out, hdc) =
let
val ctag = hdc.ctag()
in//let
if
(ctag > 0)
then fprint(out, ctag)
else fprint(out, hdc.sym())
end // end of [xemit01_h0con]
(* ****** ****** *)

local

(* ****** ****** *)

fun
auxnone
( out
: FILEref
, hdc: h0cst): void =
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
, hdc: h0cst): void =
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
, hdc: h0cst
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
, hdc: h0cst
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
, hdc: h0cst
, gnm: g1nam): void =
(
  auxsome_nil(out, hdc, gnm)
)

and
auxsome_rest
( out
: FILEref
, hdc: h0cst
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
xemit01_h0cst
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
end // end of [xemit01_h0cst]
//
end // end of [local]

(* ****** ****** *)
implement
xemit01_l1cst
(out, l1c) =
{
val () =
xemit01_h0cst
(out, l1c.hdc())
(*
val () =
fprint!
(out, "_", stmp, "_")
*)
} where
{
  val stmp = l1c.stamp()
} (*where*) // [xemit01_l1cst]
(* ****** ****** *)
implement
xemit01_l1con
(out, l1c) =
(
case+ l1c of
| L1CONcon(hdc) =>
  xemit01_h0con(out, hdc)
| L1CONval(l1v) =>
  xemit01_l1val(out, l1v)
) (* end of [xemit01_l1con] *)
(* ****** ****** *)
//
implement
xemit01_lvnam
(out, nam) =
{
val () = fprint(out, nam)
}
//
(* ****** ****** *)
//
implement
xemit01_lvtop
(out, tok) =
fprint(out, "XATS2CC_top")
//
(* ****** ****** *)
//
implement
xemit01_lvi00
( out
, int) = fprint(out, int)
implement
xemit01_lvb00
( out
, btf) = fprint(out, btf)
//
(* ****** ****** *)
//
implement
xemit01_lvint
(out, tok) =
let
val
tnd = tok.node()
in
//
case- tnd of 
|
T_INT1
( rep ) => fprint(out, rep)
//
end // end of [xemit01_lvint]
//
(* ****** ****** *)
//
implement
xemit01_lvbtf
(out, tok) =
let
val
tnd = tok.node()
in
//
case- tnd of 
|
T_IDENT_alp
( rep ) => fprint(out, rep)
//
end // end of [xemit01_lvbtf]
//
(* ****** ****** *)
//
implement
xemit01_lvchr
(out, tok) =
let
val
tnd = tok.node()
in
//
case- tnd of 
|
T_CHAR_char
( rep ) => // FIXME?
(
fprint!
( out
, "XATS2CC_char(", rep, ")")
)
|
T_CHAR_slash // FIXME?
( rep ) =>
(
fprint!
( out
, "XATS2CC_char(", rep, ")")
)
//
end // end of [xemit01_lvchr]
(* ****** ****** *)
//
implement
xemit01_lvflt
(out, tok) =
let
val
tnd = tok.node()
in
//
case- tnd of 
|
T_FLT1(rep) =>
  fprint(out, rep) // HX: FIXME!!!
//
end // end of [xemit01_lvstr]
(* ****** ****** *)
//
implement
xemit01_lvstr
(out, tok) =
let
val
tnd = tok.node()
in
//
case- tnd of 
|
T_STRING_closed
  (rep) =>
  fprint(out, rep) // HX: FIXME!!!
//
end // end of [xemit01_lvstr]
(* ****** ****** *)
//
implement
xemit01_l1exn
(out, exn0) =
let
val
stm = exn0.stamp()
in
  fprint!(out, "exn", stm)
end // end of [let]
//
(* ****** ****** *)
//
implement
xemit01_l1tmp
(out, tmp0) =
let
val arg = tmp0.arg()
in
//
if
(arg > 0)
then
let
val lev = tmp0.lev()
in
fprint!
(out, "a", lev, "x", arg)
end // end of [then]
else
let
val lev = tmp0.lev()
val stm = tmp0.stamp()
in
//
if
(lev > 0)
then
fprint!(out, "xtmp", stm)
else
fprint!(out, "xtop", stm)
//
end // end of [else]
//
end // end of [xemit01_l1tmp]
//
(* ****** ****** *)
implement
xemit01_l1val
(out, l1v0) =
(
case+
l1v0.node() of
//
|
L1VALi00(int) =>
xemit01_lvi00(out, int)
|
L1VALb00(btf) =>
xemit01_lvb00(out, btf)
(*
|
L1VALs00(str) =>
xemit01_lvs00(out, str)
*)
//
|
L1VALint(tok) =>
xemit01_lvint(out, tok)
|
L1VALbtf(tok) =>
xemit01_lvbtf(out, tok)
|
L1VALchr(tok) =>
xemit01_lvchr(out, tok)
|
L1VALstr(tok) =>
xemit01_lvstr(out, tok)
|
L1VALflt(tok) =>
xemit01_lvflt(out, tok)
//
|
L1VALtop(tok) =>
xemit01_lvtop(out, tok)
//
|
L1VALnam(nam) =>
xemit01_lvnam(out, nam)
//
|
L1VALexn(exn1) =>
xemit01_l1exn(out, exn1)
|
L1VALtmp(tmp1) =>
xemit01_l1tmp(out, tmp1)
//
|
L1VALcon(l1c1) =>
xemit01_l1con(out, l1c1)
//
|
L1VALcfun(hdc1) =>
xemit01_h0cst(out, hdc1)
|
L1VALctmp(_, _) =>
(
  aux_l1cst( out, l1v0 )
)
//
|
L1VALvfix(hdv1) =>
xemit01_h0var(out, hdv1)
//
|
L1VALctag(l1v1) =>
{
  val () =
  fprint
  ( out, "XATS2CC_ctag(" )
  val () =
  xemit01_l1val(out, l1v1)
  val () = fprint(out, ")")
}
|
L1VALcarg(l1v1, argi) =>
{
  val () =
  fprint
  (out, "XATS2CC_carg(")
  val () =
  xemit01_l1val(out, l1v1)
  val () =
  fprint!
  (out, ", ", argi+1, ")")
}
|
L1VALcofs(l1v1, idx2) =>
{
  val () =
  fprint
  ( out
  , "XATS2CC_new_cofs(")
  val () =
  xemit01_l1val(out, l1v1)
  val () =
  fprint!(out, ",", idx2+1, ")")
}
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
//
fun
fdef
( limp
: l1implmnt3): l1valopt =
let
val+
L1IMPLMNT3(rcd) = limp
in
case+
rcd.lfg of
| list_nil() => rcd.def | _ => None()
end
//
fun
aux_l1cst
( out
: FILEref
, l1v0: l1val): void =
let
//
val-
L1VALctmp
( l1c1, ldcl ) = l1v0.node()
val-
L1DCLtimpcst3
( l1c1, ldcl ) = ldcl.node()
//
in
case+
ldcl.node() of
|
L1DCLimplmnt3
(_, _, limp) =>
let
val opt = fdef(limp)
in
case+ opt of
| None() =>
  xemit01_l1cst(out, l1c1)
| Some(l1v1) =>
  xemit01_l1val(out, l1v1)
end
| _ (*else*) => xemit01_l1cst(out, l1c1)
end // end of [aux_l1cst]
//
} (*where*) // end of [xemit01_l1val]

(* ****** ****** *)
//
implement
xemit01_l1pck
( out, pck1 ) =
{
  val () = fprintln!(out, pck1)
}
//
(* ****** ****** *)

fun
xemit01_l1pcklst
( out
: FILEref
, icas: int
, tcas: l1tmp
, pcks: l1pcklst): void =
let
//
fun
auxpck0
(pck0: l1pck) : void =
(
case+ pck0 of
|
L1PCKany() =>
fprintln!
(out, "//", pck0, ";")
//
|
L1PCKi00(int, l1v) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_int00( out, int )
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_l1val( out, l1v )
val () =
xemit01_txtln(out, ") break;")
}
|
L1PCKb00(btf, l1v) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_btf00( out, btf )
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_l1val( out, l1v )
val () =
xemit01_txtln(out, ") break;")
}
//
|
L1PCKint(int, l1v) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_lvint( out, int )
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_l1val( out, l1v )
val () =
xemit01_txtln(out, ") break;")
}
//
//
|
L1PCKbtf(btf, l1v) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_lvbtf( out, btf )
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_l1val( out, l1v )
val () =
xemit01_txtln(out, ") break;")
}
//
|
L1PCKchr(chr, l1v) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_lvchr( out, chr )
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_l1val( out, l1v )
val () =
xemit01_txtln(out, ") break;")
}
//
|
L1PCKstr(str, l1v) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_lvstr( out, str )
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_l1val( out, l1v )
val () =
xemit01_txtln(out, ") break;")
}
//
|
L1PCKcon(l1c1, l1v2) =>
{
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_l1con( out, l1c1 )
val () =
xemit01_txt00( out, "!=" )
val () =
xemit01_l1val( out, l1v2 )
val () =
xemit01_txtln(out, ") break;")
}
//
|
L1PCKapp(pck1, pcks) =>
{
  val () = auxpck0(pck1)
  val () = auxpcks(pcks)
}
//
|
L1PCKtup(knd0, pcks) =>
{
  val () = auxpcks(pcks)
}
//
|
L1PCKgexp(l1v1, blk1) =>
{
val () =
xemit01_l1blk(out, blk1)
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_l1val(out, l1v1)
val () =
xemit01_txt00(out, "!=")
val () =
xemit01_txt00(out, "true")
val () =
xemit01_txtln(out, ") break;")
}
//
|
L1PCKgpat(pck1, pcks) =>
{
  val () = auxpck0(pck1)
  val () = auxpcks(pcks)
}
//
| _ (* else *) =>
{
val () =
fprintln!(out, "//", pck0, ";")
}
)
//
and
auxpcks
(pcks: l1pcklst): void =
(
case+ pcks of
|
list_nil() => ()
|
list_cons(pck1, pcks) =>
{
  val () = auxpck0( pck1 )
  val () = auxpcks( pcks )
}
)
in
//
case+ pcks of
|
list_nil() => ()
|
list_cons
(pck1, pcks) =>
let
val () =
xemit01_txtln
( out, "do {" )
//
val () = auxpck0(pck1)
//
val () =
xemit01_l1tmp(out, tcas)
val () =
fprint!
(out, " = ", icas, ";\n")
val () =
xemit01_txtln
( out, "} while(0==1);" )
val () =
xemit01_txt00(out, "if(")
val () =
xemit01_l1tmp( out, tcas )
val () =
xemit01_txt00( out, " > 0) break;\n")
//
in
  xemit01_l1pcklst
  (out, icas+1, tcas, pcks)
end (*let*)
//
end (*let*) // end of [xemit01_pcklst]

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
( l1c0 ) = l1f0.node()
in
xemit01_l1con(out, l1c0)
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
} (*where*) // end of [aux_blk]
//
fun
aux_dcl
( out
: FILEref
, lcmd
: l1cmd): void =
let
val () =
fprint!
(out, "//L1CMDdcl(...)\n")
in
// xemit01_l1dcl(out, ldcl)
end where
{
val-
L1CMDdcl(ldcl) = lcmd.node()
} (*where*) // end of [aux_dcl]
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

local

(* ****** ****** *)

#define
auxpcklst
xemit01_l1pcklst

(* ****** ****** *)

fun
auxblklst
( out
: FILEref
, icas: int
, tcas: l1tmp
, blks: l1blklst): void =
let
//
fun
auxblk0
( out
: FILEref
, blk1
: l1blk ) : void =
xemit01_l1blk(out, blk1)
//
in
case+ blks of
|
list_nil() => ()
|
list_cons(blk1, blks) =>
let
val () =
fprint!
(out, "case ", icas, ":\n")
val () = auxblk0(out, blk1)
val () =
xemit01_txt00(out, "break;\n")
in
auxblklst(out, icas+1, tcas, blks)
end (* end-of-let *)
end (* end-of-let *) // end of [auxblklst]

(* ****** ****** *)

in(* in-of-local*)

(* ****** ****** *)

fun
aux_case
( out
: FILEref
, lcmd
: l1cmd): void =
{
//
val () =
xemit01_txt00(out, "{\n")
//
val () =
xemit01_l1tmp(out, tcas)
val () =
xemit01_txtln(out, " = 0;")
val () =
xemit01_txt00(out, "do {\n")
val () =
auxpcklst(out, 1(*i*), tcas, pcks)
val () =
fprint!( out, "} while(false);\n" )
//
val () =
fprintln!( out, "} // case-patck0" )
//
val () =
xemit01_txt00
(out, "switch\n(")
val () =
xemit01_l1tmp(out, tcas)
val () =
xemit01_txt00(out, ") {\n")
//
val () =
auxblklst(out, 1(*i*), tcas, blks)
//
val () =
fprint!
( out
, "default: XATS2CC_matcherr0();\n")
val () =
xemit01_txtln(out, "} // case-switch")
//
} where
{
//
  val-
  L1CMDcase
  ( knd0
  , l1v1
  , tcas
  , pcks
  , blks) = lcmd.node((*void*))
//
} (* where *) // end of [aux_case]

(* ****** ****** *)

fun
aux_try0
( out
: FILEref
, lcmd
: l1cmd): void =
{
//
val () =
fprint!(out, "try\n{\n")
val () =
xemit01_l1blk(out, blk1)
val () =
fprint!(out, "}//try\n")
val () =
fprint!(out, "catch\n(")
val () =
xemit01_l1exn(out, texn)
val () =
xemit01_txt00(out, ") {\n")
//
val () =
xemit01_l1tmp(out, tcas)
val () =
xemit01_txtln(out, " = 0;")
val () =
xemit01_txt00(out, "do {\n")
val () =
auxpcklst(out, 1(*i*), tcas, pcks)
val () =
fprint!( out, "} while(false);\n" )
//
val () =
xemit01_txt00
(out, "switch\n(")
val () =
xemit01_l1tmp(out, tcas)
val () =
xemit01_txt00(out, ") {\n")
//
val () =
auxblklst(out, 1(*i*), tcas, blks)
//
val () =
fprint!
(out, "default: ")
val () =
fprint!
(out, "XATS2CC_reraise(")
val () =
xemit01_l1exn( out, texn )
val () = fprint!(out, ");\n")
//
val () =
xemit01_txtln(out, "} // with-switch")
val () =
xemit01_txtln(out, "} // try0-with-catch")
//
} where
{
//
  val-
  L1CMDtry0
  ( blk1
  , texn
  , tcas
  , pcks
  , blks) = lcmd.node((*void*))
//
} (* where *) // end of [aux_try0]

(* ****** ****** *)

end (* end-of-local *) 

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
L1CMDdcl _ => aux_dcl(out, lcmd)
//
|
L1CMDift1 _ => aux_ift1(out, lcmd)
//
|
L1CMDcase _ => aux_case(out, lcmd)
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
xemit01_l0fag
( out, tmps ) =
{
val () =
xemit01_txt00(out, "(")
val () =
auxlst(out, 0(*i0*), tmps)
val () =
xemit01_txt00(out, ")")
} where
{
fun
auxlst
( out
: FILEref
, i0: int
, tmps: l1tmplst): void =
(
case+ tmps of
|
list_nil() => ()
|
list_cons(tmp1, tmps) =>
{
  val
  l1t1 = tmp1.ltnm()
//
  val () =
  if
  (i0 > 0)
  then
  xemit01_txt00
  ( out, ", " )
  val () =
  xemit01_txt00
  (out, "_xarg_(")
  val () =
  xemit01_l1tmp(out, tmp1)
  val () =
  xemit01_txt00(out, ", ")
  val () =
  xemit01_l1tnm(out, l1t1)
  val () =
  xemit01_txt00( out, ")" )
//
  val () = auxlst(out, i0+1, tmps)
//
}
) (*case*) // end of [auxlst]
} (*where*) // end of [xemit01_l0fag]

implement
xemit01_l0faglst
  ( out, lfgs ) =
(
case+ lfgs of
|
list_nil() => ()
|
list_cons(lfg1, lfgs) =>
let
  val () =
  xemit01_l0fag(out, lfg1)
in
  xemit01_l0faglst(out, lfgs)
end
) (*case*) // end of [xemit01_l0faglst]

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
} (*where*) // end of [xemit01_fargdecs]

(* ****** ****** *)

implement
xemit01_ftmpdecs
( out, tmps ) =
(
case+ tmps of
|
list_nil() => ()
|
list_cons(x1, xs) =>
let
val i0 = x1.arg()
in(* in-of-let *)
if
(i0 > 0)
then
(
xemit01_ftmpdecs(out, xs)
)
else
(
xemit01_ftmpdecs(out, xs)
) where
{
//
(*
val () =
xemit01_txt00
(out, "let ") // local
*)
val () =
xemit01_txt00
(out, "_xtmp_(")
//
val () =
xemit01_l1tmp( out, x1 )
//
val () =
let
val l1t = x1.ltnm()
in
xemit01_txt00( out, "," )
;
xemit01_l1tnm( out, l1t )
end
//
val () =
xemit01_txt00( out, ");" )
//
val () = xemit01_newln( out )
//
} (* end of [else] *)
end // let // end of [list_cons]
) (*case*) // end of [xemit01_ftmpdecs]

(* ****** ****** *)
//
local

fun
aux_fundclst
( out
: FILEref
, dcl0: l1dcl): void =
let
val-
L1DCLfundclst
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
end // end of [aux_fundclst]

(* ****** ****** *)

fun
aux_valdclst
( out
: FILEref
, dcl0: l1dcl): void =
let
//
fun
auxlvd0
( lvd0
: l1valdecl): void =
{
//
val+
L1VALDECL(rcd) = lvd0
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
: l1valdeclist): void =
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
L1DCLvaldclst
( knd0
, mopt
, lvds) = dcl0.node() in auxlvds(lvds)
end
end // end of [aux_valdclst]

(* ****** ****** *)

fun
aux_datatype
( out
: FILEref
, dcl0: l1dcl): void =
let
//
fun
auxhtcs1
( htcs
: htcstlst): void =
(
case+ htcs of
|
list_nil() => ()
|
list_cons(htc1, htcs) =>
(
  auxhtcs1(htcs)) where
{
  val () =
  xemit01_htdat1(out, htc1)
}
) (* end of [auxhtcs1] *)
//
fun
auxhtcs2
( htcs
: htcstlst): void =
(
case+ htcs of
|
list_nil() => ()
|
list_cons(htc1, htcs) =>
(
  auxhtcs2(htcs)) where
{
  val () =
  xemit01_htdat2(out, htc1)
}
) (* end of [auxhtcs1] *)
//
in
let
val-
L1DCLdatatype
( htcs ) = dcl0.node()
in
auxhtcs1(htcs); auxhtcs2(htcs)
end
end // end of [aux_datatype]

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
L1DCLfundclst _ =>
{
val()=aux_fundclst(out, dcl0)
}
//
|
L1DCLvaldclst _ =>
{
val()=aux_valdclst(out, dcl0)
}
//
(*
|
L1DCLvardecl _ =>
{
val()=aux_vardecl(out, dcl0)
}
*)
//
|
L1DCLdatatype _ =>
{
val ()=aux_datatype(out, dcl0)
}
//
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
: l1fundecl): void =
let
//
val+
L1FUNDECL(rcd) = lfd0
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
: l1fundecl): void =
let
//
val+
L1FUNDECL(rcd) = lfd0
//
val () =
xemit01_txtln
(out, "// function")
//
val () =
(
case-
rcd.rtp of
|
Some(rtyp) =>
xemit01_l1tnm( out, rtyp )
)
val () = xemit01_newln(out)
//
val () =
xemit01_h0cst(out, rcd.hdc)
//
(*
val narg =
(
case+
rcd.hfg of
|
None() => 0
|
Some
(rcd_hfg) => narg where
{
val
narg =
xemit01_hfarglst
( out
, rcd.lev
, rcd_hfg, 0(*base*) )
val () = xemit01_newln(out)
}
)
*)
//
val () =
(
case+
rcd.lfg of
|
None () => ()
|
Some
(rcd_lfg) =>
{
  val () =
  xemit01_l0faglst
  ( out, rcd_lfg )
  val () =
  xemit01_newln(out)
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
xemit01_l1blk(out, rcd.lfg_blk)
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
: l1fundeclist): void =
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
L1DCLfundclst
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

(* end of [xats_intrep1_xemit0.dats] *)
