(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Xanadu - Unleashing the Potential of Types!
** Copyright (C) 2022 Hongwei Xi, ATS Trustful Software, Inc.
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
// Start Time: January, 2022
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
#symload
fprint with $STM.fprint_stamp
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
#staload "./../SATS/tcomp01.sats"
(* ****** ****** *)
extern
fun
tcomp01_h0pat(h0pat): void
extern
fun
tcomp01_h0patlst(h0patlst): void
(* ****** ****** *)
extern
fun
tcomp01_h0fag(h0fag): void
extern
fun
tcomp01_h0faglst(h0faglst): void
(* ****** ****** *)
extern
fun
tcomp01_h0exp(h0exp): void
extern
fun
tcomp01_h0explst(h0explst): void
extern
fun
tcomp01_h0expopt(h0expopt): void
(* ****** ****** *)
extern
fun
tcomp01_h0gpat(h0gpat): void
extern
fun
tcomp01_h0gualst(h0gualst): void
(* ****** ****** *)
extern
fun
tcomp01_h0clau(h0clau): void
extern
fun
tcomp01_h0claulst(h0claulst): void
(* ****** ****** *)
//
implement
fprint_val<h0typ> = fprint_h0typ
//
(* ****** ****** *)
//
implement
fprint_val<h0pat> = fprint_h0pat
implement
fprint_val<h0fag> = fprint_h0fag
//
implement
fprint_val<h0exp> = fprint_h0exp
//
(* ****** ****** *)
implement
fprint_val<htqarg> = fprint_htqarg
implement
fprint_val<htiarg> = fprint_htiarg
(* ****** ****** *)

implement
tcomp01_h0pat
  ( h0p0 ) =
let
//
val
loc0 = h0p0.loc()
val
h0t0 = h0p0.type()
//
val
ltnm = h0typ_tnmize(h0t0)
//
in//in-of-let
//
case+
h0p0.node() of
//
|
H0Pvar(hdv1) =>
println!("H0Pvar: ", ltnm)
//
|
H0Pcon(hdc1) =>
println!("H0Pcon: ", ltnm)
//
|
H0Pdapp
(h0f0, npf1, h0ps) =>
let
val () =
println!("H0Pdapp: ", ltnm)
in
tcomp01_h0pat(h0f0);
tcomp01_h0patlst(h0ps)
end (*let*)//end-of-[H0Pdapp]
//
|
_(* rest-of-h0pat*) =>
println!
("tcomp01_h0pat: h0p0 = ", h0p0)
//
end // end of [tcomp01_h0pat]

implement
tcomp01_h0patlst
  ( h0ps ) =
(
case+ h0ps of
|
list_nil() => ()
|
list_cons(h0p1, h0ps) =>
{
val () = tcomp01_h0pat(h0p1)
val () = tcomp01_h0patlst(h0ps)
}
)(*case*)//end-of-[tcomp01_h0patlst]

(* ****** ****** *)
//
implement
tcomp01_h0fag
  ( hfa0 ) =
(
case+
hfa0.node() of
//
| H0FAGnone0() => ()
| H0FAGnone1(_) => ()
//
| H0FAGnpats
  (npf1, h0ps) =>
(
  tcomp01_h0patlst( h0ps )
)
//
) (*case*)//end-of-[tcomp01_h0fag]
//
implement
tcomp01_h0faglst
  ( hfas ) =
(
case+ hfas of
|
list_nil() => ()
|
list_cons(hfa1, hfas) =>
{
val () = tcomp01_h0fag(hfa1)
val () = tcomp01_h0faglst(hfas)
}
)(*case*)//end-of-[tcomp01_h0faglst]
//
(* ****** ****** *)
//
implement
tcomp01_h0exp
  ( h0e0 ) =
let
val loc0 = h0e0.loc()
val h0t0 = h0e0.type()
//
val
ltnm = h0typ_tnmize(h0t0)
//
in//in-of-let
//
case+
h0e0.node() of
//
|
H0Eint _ =>
println!("H0Eint: ", ltnm)
|
H0Ebtf _ =>
println!("H0Ebtf: ", ltnm)
|
H0Echr _ =>
println!("H0Echr: ", ltnm)
|
H0Estr _ =>
println!("H0Estr: ", ltnm)
//
|
H0Evar(hdv1) =>
println!("H0Evar: ", ltnm)
|
H0Ekvar(knd0, hdv1) =>
println!("H0Ekvar: ", ltnm)
|
H0Efcst(hdc1) =>
println!("H0Efcst: ", ltnm)
//
|
H0Etcst
(hdc1, tiarg) =>
println!("H0Etcst: ", ltnm)
//
|
H0Etimp
( stmp
, h0e1, targ
, hdcl, tsub) =>
(
tcomp01_h0exp(h0e1);
tcomp01_h0dcl(hdcl)
) where
{
val () =
println!("H0Etimp: ", ltnm)
val () =
println!("H0Etimp: targ = ", targ)
val () =
println!("H0Etimp: tsub = ", tsub)
} (*where*)//end of [H0Etimp]
//
|
H0Edapp
(h0f0, npf1, h0es) =>
(
tcomp01_h0exp(h0f0);
tcomp01_h0explst(h0es)
) where
{
val () =
println!("H0Edapp: ", ltnm)
}(*where*) // end of [H0Edapp]
//
|
H0Elet(dcls, h0e1) =>
(
tcomp01_h0exp(h0e1)
) where
{
val () =
tcomp01_h0dclist(dcls)
} where
{
val () =
println!("H0Elet: ", ltnm)
}
//
|
H0Eift1
(h0e1, h0e2, opt3) =>
(
  tcomp01_h0exp(h0e1)
; tcomp01_h0exp(h0e2)
; tcomp01_h0expopt(opt3)
) where
{
val () =
println!("H0Eift1: ", ltnm)
}
//
|
H0Ecase
(knd0, h0e1, hcls) =>
(
tcomp01_h0exp(h0e1);
tcomp01_h0claulst(hcls)
) where
{
val () =
println!("H0Ecase: ", ltnm)
}
//
|
_(* rest-of-h0exp*) =>
println!
("tcomp01_h0exp: h0e0 = ", h0e0)
//
end(*let*)//end-of-[tcomp01_h0exp]
//
(* ****** ****** *)

implement
tcomp01_h0explst
  ( h0es ) =
(
case+ h0es of
|
list_nil() => ()
|
list_cons(h0e1, h0es) =>
{
val () = tcomp01_h0exp(h0e1)
val () = tcomp01_h0explst(h0es)
}
)(*case*)//end-of-[tcomp01_h0explst]

(* ****** ****** *)

implement
tcomp01_h0expopt
  ( opt0  ) =
(
case+ opt0 of
| None() => ()
| Some(h0e1) =>
{
  val () = tcomp01_h0exp(h0e1)
}
)(*case*)//end-of-[tcomp01_h0expopt]

(* ****** ****** *)

implement
tcomp01_h0gpat
  ( hgpt ) =
(
case+
hgpt.node() of
|
H0GPATpat(h0p1) =>
tcomp01_h0pat(h0p1)
|
H0GPATgua(h0p1, h0gs) =>
{
val () = tcomp01_h0pat(h0p1)
val () = tcomp01_h0gualst(h0gs)
}
)(*case*)//end-of-[tcomp01_h0gpat]

(* ****** ****** *)

local

fun
auxgua
(h0g0: h0gua): void =
(
case+
h0g0.node() of
|
H0GUAexp(h0e1) =>
tcomp01_h0exp(h0e1)
|
H0GUAmat(h0e1, h0p2) =>
{
val () = tcomp01_h0exp(h0e1)
val () = tcomp01_h0pat(h0p2)
}
) (*case*) // end of [auxgua]

in(*in-of-local*)

implement
tcomp01_h0gualst
  ( h0gs  ) =
(
case+ h0gs of
|
list_nil() => ()
|
list_cons(h0g1, h0gs) =>
{
val () = auxgua(h0g1)
val () = tcomp01_h0gualst(h0gs)
}
)(*case*)//end-of-[tcomp01_h0gualst]

end // end of [local]

(* ****** ****** *)
//
implement
tcomp01_h0clau
  ( hcl0 ) =
(
case+
hcl0.node() of
|
H0CLAUpat
( hgpt ) =>
tcomp01_h0gpat(hgpt)
|
H0CLAUexp
(hgpt, h0e1) =>
(
  tcomp01_h0exp(h0e1)
) where
{
val () = tcomp01_h0gpat(hgpt)
}
)(*case*)//end-of-[tcomp01_h0clau]
//
implement
tcomp01_h0claulst
  ( hcls ) =
(
case+ hcls of
|
list_nil() => ()
|
list_cons(hcl1, hcls) =>
{
val () = tcomp01_h0clau(hcl1)
val () = tcomp01_h0claulst(hcls)
}
)(*case*)//end-of-[tcomp01_h0claulst]
//
(* ****** ****** *)
//
extern
fun
tcomp01_h0valdecl
  ( hvd0: h0valdecl ) : void
extern
fun
tcomp01_h0valdeclist
  (hvds: h0valdeclist): void
//
extern
fun
tcomp01_h0vardecl
  ( hvd0: h0vardecl ) : void
extern
fun
tcomp01_h0vardeclist
  (hvds: h0vardeclist): void
//
extern
fun
tcomp01_h0fundecl
  ( hfd0: h0fundecl ) : void
extern
fun
tcomp01_h0fundeclist
  (hfds: h0fundeclist): void
//
(* ****** ****** *)

local

(* ****** ****** *)

fun
aux_valdclst
(dcl0: h0dcl): void =
let
val-
H0Cvaldclst
( knd0
, mopt
, hvds) = dcl0.node()
in
tcomp01_h0valdeclist(hvds)
end(*let*)//end of [aux_valdclst]

(* ****** ****** *)

fun
aux_fundclst
(dcl0: h0dcl): void =
let
//
val-
H0Cfundclst
( knd0
, mopt
, tqas
, hfds) = dcl0.node()
//
val () =
println!
("H0Cfundclst: tqas = ", tqas)
//
in
  tcomp01_h0fundeclist(hfds)
end(*let*)//end-of-[aux_fundclst]

(* ****** ****** *)

fun
aux_implmnt3
(dcl0: h0dcl): void =
let
//
val-
H0Cimplmnt3
( tok0
, stm0
, mopt
, htqa
, hdc0, htia
, hfas, h0e1) = dcl0.node()
//
val ( ) =
println!
("HIMPDECL3.hdc0 = ", hdc0)
//
val ( ) =
println!
("HIMPDECL3.htqa = ", htqa)
val ( ) =
println!
("HIMPDECL3.htia = ", htia)
//
val ( ) =
println!
("HIMPDECL3.hfas = ", hfas)
val ( ) = tcomp01_h0faglst(hfas)
//
in
tcomp01_h0exp(h0e1) where
{
val ( ) =
println!("HIMPDECL3.body = ", h0e1)
}
end // end of [aux_implmnt3]

(* ****** ****** *)

in(* in-of-local *)

implement
tcomp01_h0dcl
  ( dcl0 ) =
(
case+
dcl0.node() of
//
|
H0Cvaldclst _ =>
aux_valdclst( dcl0 )
//
|
H0Cfundclst _ =>
aux_fundclst( dcl0 )
//
|
H0Cimplmnt3 _ =>
aux_implmnt3( dcl0 )
//
| _ (*rest-of-h0dcl*) =>
{
val () =
println!
("tcomp01_h0dcl: dcl0 = ", dcl0)
}
//
) where
{
(*
val () =
println!
("tcomp01_h0dcl: dcl0 = ", dcl0)
*)
//
} (*where*)//end of [tcomp01_h0dcl]

end // end of [local]

(* ****** ****** *)

implement
tcomp01_h0dclist
  ( dcls ) =
(
case+ dcls of
|
list_nil() => ()
|
list_cons(dcl1, dcls) =>
{
val () = tcomp01_h0dcl(dcl1)
val () = tcomp01_h0dclist(dcls)
}
)(*case*)//end of [tcomp01_h0dclist]

(* ****** ****** *)

implement
tcomp01_h0valdecl
  ( x0 ) =
let
//
val+
H0VALDECL
( rcd ) = x0
//
(*
val loc0 = rcd.loc
*)
val pat1 = rcd.pat
val def2 = rcd.def
//
val () =
println!
("H0VALDECL.pat = ", pat1)
val () =
println!
("H0VALDECL.def = ", def2)
//
val () = tcomp01_h0pat(pat1)
//
in
//
case+ def2 of
| None() => ()
| Some(h0e2) => tcomp01_h0exp(h0e2)
//
end (*let*)//end-of-[tcomp01_h0valdecl]

implement
tcomp01_h0valdeclist
  ( xs ) =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  tcomp01_h0valdeclist(xs)
) where
{
val () = tcomp01_h0valdecl(x0)
}
)(*case*)//end-of-[tcomp01_h0valdeclist]

(* ****** ****** *)

implement
tcomp01_h0fundecl
  ( x0 ) =
let
//
val+
H0FUNDECL
  (rcd) = x0
//
(*
val loc0 = rcd.loc
*)
val hdc0 = rcd.hdc
val hfg1 = rcd.hfg
val def2 = rcd.def
//
val () =
println!
( "H0FUNDECL.hdc = ", hdc0 )
//
val () =
(
case hfg1 of
|
None() =>
println!
("H0FUNDECL.hfg = ", "None(", ")" )
|
Some(hfas) =>
(
  tcomp01_h0faglst(hfas)
) where
{
val () =
println!
("H0FUNDECL.hfg = ", "Some(", hfas, ")")
}
) (*case*) // end-of-val
//
val () =
println!(   "H0FUNDECL.def = ", def2   )
//
in
//
case+ def2 of
|
None() => ()
|
Some(h0e2) => tcomp01_h0exp(h0e2)
//
end (*let*) // end of [tcomp01_h0fundecl]

implement
tcomp01_h0fundeclist
  ( xs ) =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  tcomp01_h0fundeclist(xs)
) where
{
  val () = tcomp01_h0fundecl(x0)
}
) (*case*)//end of [tcomp01_h0fundeclist]

(* ****** ****** *)

local

(* ****** ****** *)
val
out = stdout_ref
(* ****** ****** *)
//
fun
auxltnm
(ltnm: l1tnm): void =
let
val lctp = ltnm.lctp()
in//in-of-let
case+ lctp of 
|
L1CTPname _ => aux_name(ltnm)
|
L1CTPtydat _ => aux_tydat(ltnm)
//
|
_(*rest-of-L1CTP...*) =>
let
val stmp = ltnm.stamp()
in(*in-of-let*)
fprintln!
( out, "// ", "L1TNM_", stmp, " = ", lctp, ";")
end // end of [let]
end (*let*) // end of [auxltnm]
//
and
aux_name
(ltnm: l1tnm): void =
let
//
val stmp = ltnm.stamp()
//
val-
L1CTPname(name) = ltnm.lctp()
//
in(*in-of-let*)
//
fprintln!
(out, "typedef ", name, " ", "L1TNM_", stmp, ";")
//
end (*let*) // end of [aux_name]
//
and
aux_tydat
(ltnm: l1tnm): void =
let
//
val stmp = ltnm.stamp()
//
in(*in-of-let*)
(
f_l1dtclst(dtcs) ) where
{
//
val-
L1CTPtydat
(htc1, h0ts, dtcs) = ltnm.lctp()
//
fun
f_l1ctp
(lctp: l1ctp): void =
(
case lctp of
|
L1CTPltnm(ltnm) =>
fprint!(out, "L1TNM_", ltnm.stamp())
| _(*non-L1CTPltnm*) => fprint!(out, lctp)
)
//
and
f_l1dtc
(ldtc: l1dtc): void =
let
val+
L1DTCdtcon
(hdc1, l1ts) = ldtc
val ctag = hdc1.ctag()
in
fprint!
( out
, "typedef "
, "struct{ ", "xcmp_ctag_t ctag0; ");
f_carglst(1, l1ts);fprint!( out,  "} ");
fprintln!(out, "*L1TNM_", stmp, "_", ctag, ";")
end (*let*) // end of [f_l1dtc]
//
and
f_carglst
( i0: int
, l1ts: l1ctplst): void =
(
case+ l1ts of
|
list_nil() => ()
|
list_cons(l1t1, l1ts) =>
(
f_carglst(i0+1, l1ts)) where
{
val () =
(
f_l1ctp(l1t1);fprint!(out, " ", "carg", i0, "; "))
}
) (*case*)//end of [f_carglst]
//
and
f_l1dtclst
(dtcs: l1dtclst): void =
(
case+ dtcs of
|
list_nil() => ()
|
list_cons(ldtc, dtcs) =>
let
val () = f_l1dtc(ldtc) in f_l1dtclst(dtcs)
end
) (*case*)//end of [f_l1dtclst]
//
val () =
fprintln!
(out, "// ", "L1TNM_", stmp, " = ", ltnm.lctp())
//
val () =
fprintln!
( out
, "typedef "
, "struct{ xcmp_ctag_t ctag0; } ", "*L1TNM_", stmp, ";")
//
} (*where*)
//
end (*let*) // end of [aux_tydat]
//
(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

implement
tcomp01_the_ltnmmap
  ( (*void*) ) =
(
  auxmain(l1ts)) where
{
//
val
l1ts =
the_ltnmmap_listize()
//
fun
auxmain
( tmns
: l1tnmlst_vt): void =
(
case+ tmns of
| ~
list_vt_nil
( (*void*) ) => ()
| ~
list_vt_cons
( tnm1, tnms ) =>
(
  auxmain(tnms)) where
{
//
val () = auxltnm(tnm1)
//
(*
val () =
fprintln!
(out, "tcomp01_the_ltnmmap: ", tnm1)
*)
//
}
)
//
}(*where*)//end-of[tcomp01_the_ltnmmap]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [xats_tcomp01_dynexp.dats] *)
