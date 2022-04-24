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
// Start Time: February, 2022
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
#staload $S2E(*open-pkg*)
(* ****** ****** *)
#staload $INTREP0(* open *)
(* ****** ****** *)
#staload "./../SATS/intrep1.sats"
(* ****** ****** *)
#symload
compare
with $STM.cmp_stamp_stamp
#symload
compare
with $SYM.cmp_symbol_symbol
(* ****** ****** *)
#symload
fprint with $STM.fprint_stamp
(* ****** ****** *)

fun
htcst_compare
( htc1: htcst
, htc2: htcst): int =
compare
(htc1.stamp(), htc2.stamp())
fun
htvar_compare
( htv1: htvar
, htv2: htvar): int =
compare
(htv1.stamp(), htv2.stamp())
(* ****** ****** *)
#symload compare with htcst_compare
#symload compare with htvar_compare
(* ****** ****** *)

local

(* ****** ****** *)
//
#define H0Tbas_t 10
#define H0Tcst_t 20
#define H0Tvar_t 30
#define H0Tlft_t 40
#define H0Tfun_t 50
#define H0Tapp_t 60
#define H0Tlam_t 70
#define H0Ttyext_t 80
#define H0Ttyrec_t 90
//
#define H0Tnone0_t 100
#define H0Tnone1_t 101
//
(* ****** ****** *)

in(* in-of-local *)

fun
h0typ_tag
(h0t0: h0typ): int =
(
case+
h0t0.node() of
| H0Tbas _ => H0Tbas_t
//
| H0Tcst _ => H0Tcst_t
| H0Tvar _ => H0Tvar_t
//
| H0Tlft _ => H0Tlft_t
//
| H0Tfun _ => H0Tfun_t
//
| H0Tapp _ => H0Tapp_t
| H0Tlam _ => H0Tlam_t
//
| H0Ttyext _ => H0Ttyext_t
//
| H0Ttyrec _ => H0Ttyrec_t
//
| H0Tnone0 _ => H0Tnone0_t
| H0Tnone1 _ => H0Tnone1_t
) (* end of [ht0yp_tag] *)

(* ****** ****** *)

#symload .tag with h0typ_tag

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

fun
auxteq
( h0t1: h0typ
, h0t2: h0typ): int =
(
case+
h0t1.node() of
|
H0Tbas(sym1) =>
(
case-
h0t2.node() of
|
H0Tbas(sym2) => compare(sym1, sym2)
)
//
|
H0Tcst(htc1) =>
(
case-
h0t2.node() of
|
H0Tcst(htc2) => compare(htc1, htc2)
)
//
|
H0Tvar(htv1) =>
(
case-
h0t2.node() of
|
H0Tvar(htv2) => compare(htv1, htv2)
)
//
|
H0Tlft(h0t1) =>
(
case-
h0t2.node() of
|
H0Tlft(h0t2) => compare(h0t1, h0t2)
)
//
|
H0Tfun
(npf1, hts1, h0t1) =>
(
case-
h0t2.node() of
|
H0Tfun
(npf2, hts2, h0t2) =>
let
val sgn = compare(h0t1, h0t2)
in
  if (sgn != 0)
  then sgn else auxlst( hts1, hts2 )
end
)
//
|
H0Tapp
(h0t1, hts1) =>
(
case-
h0t2.node() of
|
H0Tapp
(h0t2, hts2) =>
let
val sgn = compare(h0t1, h0t2)
in
  if (sgn != 0)
  then sgn else auxlst(hts1, hts2)
end
)
//
|
H0Tlam
(svs1, h0t1) =>
(
case-
h0t2.node() of
|
H0Tlam
(svs2, h0t2) => compare(h0t1, h0t2)
)
//
|
H0Ttyext
(nam1, hts1) =>
(
case-
h0t2.node() of
|
H0Ttyext
(nam2, hts2) =>
let
val sgn = compare(nam1, nam2)
in
  if (sgn != 0)
  then sgn else auxlst( hts1, hts2 )
end
)
//
|
H0Ttyrec
(knd1, npf1, lhts1) =>
(
case-
h0t2.node() of
|
H0Ttyrec
(knd2, npf2, lhts2) => (~1)
)
//
|
H0Tnone0 _ =>
(case- h0t2.node() of H0Tnone0 _ => 0)
|
H0Tnone1 _ =>
(case- h0t2.node() of H0Tnone1 _ => 0)
//
) (*case*) // end of [auxteq]

(* ****** ****** *)

and
auxlst
( xs: h0typlst
, ys: h0typlst): int =
(
case+ xs of
//
|
list_nil() =>
(
case+ ys of
|
list_nil() => (0)
|
list_cons _ => (~1)
)
//
|
list_cons(x1, xs) =>
(
case+ ys of
|
list_nil() => (1)
|
list_cons(y1, ys) =>
let
val sgn = compare(x1, y1)
in
if (sgn != 0) then sgn else auxlst(xs, ys)
end // end of [list_cons]
)
//
) (*case*) // end of [auxlst]

(* ****** ****** *)

in(*in-of-local*)

implement
h0typ_compare
(h0t1, h0t2) =
let
val tag1 = h0t1.tag()
val tag2 = h0t2.tag()
val cmp3 = compare(tag1, tag2)
in
if
(cmp3 = 0)
then auxteq(h0t1, h0t2) else cmp3
end

end // end of [local]

(* ****** ****** *)

implement
l1tnm_compare
(tnm1, tnm2) =
compare(tnm1.stamp(), tnm2.stamp())

(* ****** ****** *)
//
extern
fun
the_ltnmmap_search_ref
(h0t0: h0typ): P2tr0(l1tnm)
//
(* ****** ****** *)

local

(* ****** ****** *)
#staload
"libats/SATS\
/linmap_avltree.sats"
#staload _ =
"libats/DATS\
/linmap_avltree.dats"
(* ****** ****** *)

extern
prfun
lemma_p2tr_param
{a:vt0p}
{l:addr}(cp: p2tr(a, l)): [l >= null] void

(* ****** ****** *)
in(* in-of-local *)
(* ****** ****** *)

local

typedef
key = h0typ
and
itm = l1tnm
vtypedef
ltnmmap = map(key, itm)

var
the_ltnmmap =
linmap_make_nil<>{key,itm}()
val
the_ltnmmap = addr@the_ltnmmap

(* ****** ****** *)
implement
compare_key_key<key>
  (k1, k2) =
(
$effmask_all(h0typ_compare(k1, k2))
)
(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

implement
the_ltnmmap_search_ref
  (h0t0) = let
//
val
map =
$UN.ptr0_get<ltnmmap>(the_ltnmmap)
val ref =
linmap_search_ref<key,itm>(map,h0t0)
//
in
let
prval () = $UN.cast2void(map)
prval () = lemma_p2tr_param(ref) in ref
end
end // end of [the_ltnmmap_search_ref]

(* ****** ****** *)

implement
the_ltnmmap_search_opt
  (h0t0) = let
//
val
ref = the_ltnmmap_search_ref(h0t0)
//
in
//
if
iseqz(ref)
then None_vt()
else Some_vt($UN.p2tr_get<itm>(ref))
//
end // end of [the_ltnmmap_search_opt]

(* ****** ****** *)
(*
//
extern
fun
the_ltnmmap_insert_any
(h0t0: h0typ, ltnm: l1tnm): void
//
implement
the_ltnmmap_insert_any
  (h0t0, ltnm) = let
//
var
map =
$UN.ptr0_get<ltnmmap>(the_ltnmmap)
//
in
(
$UN.ptr0_set<ltnmmap>(the_ltnmmap, map)
) where
{
val () =
linmap_insert_any<key,itm>(map, h0t0, ltnm)
}
end // end of [the_ltnmmap_insert_any]
//
*)
(* ****** ****** *)

implement
the_ltnmmap_insert_exn
  (h0t0, ltnm) = let
//
var
map =
$UN.ptr0_get<ltnmmap>(the_ltnmmap)
//
in
(
$UN.ptr0_set<ltnmmap>(the_ltnmmap, map)
) where
{
val-
~None_vt() =
linmap_insert_opt<key,itm>(map, h0t0, ltnm)
}
end // end of [the_ltnmmap_insert_exn]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

fun
auxh0t0
(h0t0: h0typ): void =
(
case+
h0t0.node() of
//
| H0Tbas _ => ()
| H0Tcst _ => ()
| H0Tvar _ => ()
//
|
H0Tlft(h0t1) => auxh0t0(h0t1)
|
H0Tapp
(h0t1, h0ts) => auxh0ts(h0ts)
|
H0Tfun
(npf0,
 h0ts, h0t1) => auxh0ts(h0ts)
|
H0Tlam
(s2vs, h0t1) => auxh0t0(h0t1)
|
H0Ttyrec
(knd0,
 npf0, lhts) => auxlhts(lhts)
|
H0Ttyext
(tnm0, h0ts) => auxh0ts(h0ts)
//
| H0Tnone0() => ((*void*))
| H0Tnone1(h0t1) => ((*void*))
//
) (*case*) // end of [auxh0t0]

and
auxh0ts
(h0ts: h0typlst): void =
(
case+ h0ts of
|
list_nil() => ()
|
list_cons(h0t1, h0ts) =>
let
val _(*ltnm*) =
h0typ_tnmize(h0t1) in auxh0ts(h0ts)
end // end of [list_cons]
) (*case*) // end of [auxh0ts]

and
auxlhts
(lhts: labh0typlst): void =
(
case+ lhts of
|
list_nil() => ()
|
list_cons(lht1, lhts) =>
let
val _(*ltnm*) =
h0typ_tnmize(h0t1) in auxlhts(lhts)
end where
{
  val+SLABELED( lab1, h0t1 ) = lht1
}
) (*case*) // end of [auxlhts]

in(* in-of-local *)
//
implement
h0typ_tnmize
  ( h0t0 ) = l1tnm_make_type(h0t0)
//
implement
h0typ_tnmize_rec
  ( h0t0 ) = let
//
val
ltnm =
h0typ_tnmize(h0t0) in auxh0t0(h0t0); ltnm
//
end // end of [h0typ_tnmize_rec]
//
end // end of [local]

(* ****** ****** *)

(* end of [xats_interp1_util0.dats] *)
