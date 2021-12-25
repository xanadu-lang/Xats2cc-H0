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
static
fun
the_dvarmap_search_ref
(hdv: hdvar): P2tr0(l1valist)
//
static
fun
the_dvarmap_insert_one
(hdv: hdvar, l1v1: l1val): void
//
static
fun
the_dvarmap_remove_one(hdvar): void
//
(* ****** ****** *)
//
datavtype
hdvarstk =
|
hdvarstk_nil of
  ((*void*))
//
|
hdvarstk_fun0 of
  (hdvarstk(*rest*))
//
|
hdvarstk_cons of
  (hdvar, hdvarstk(*rest*))
//
(* ****** ****** *)
//
fun
hdvarstk_pop_top
( xs:
& hdvarstk >> _): void =
(
  xs := auxstk(xs)
) where
{
fun
auxstk
( xs
: hdvarstk): hdvarstk =
(
case+ xs of
//
| ~
hdvarstk_cons
  (x0, xs) =>
  auxstk(xs) where
{
  val () = 
  the_dvarmap_remove_one(x0)
}
//
| _ (* non-hdvarstk *) => (xs)
)
} (* end of [hdvarstk_pop_top] *)
//
(* ****** ****** *)

local

datavtype
compenv =
COMPENV of @{
  flevel= int
,
  hdvarstk= hdvarstk
}

absimpl
compenv_vtbox = compenv

in(*in-of-local*)

implement
compenv_make_nil
  ((*void*)) =
let
(*
val () =
println!("compenv_make_nil")
*)
in
//
COMPENV@{
  flevel= 0
,
  hdvarstk= hdvarstk
} where
{
  val
  hdvarstk = hdvarstk_nil()
}
//
end (*let*) // end of [compenv_make_nil]

(* ****** ****** *)

implement
compenv_free_top
  ( env0 ) =
let
val () =
free@(env0) in tmps
end where
{
//
val
tmps = list_nil()
//
val+
@COMPENV(rcd) = env0
//
val () =
hdvarstk_pop_top(rcd.hdvarstk)
//
val-( 0 ) = rcd.flevel
//
val-~hdvarstk_nil() = rcd.hdvarstk
//
} (*where*) // end of [compenv_free_top]

(* ****** ****** *)

//
implement
xcomp01_dvaradd_fun0
  (env0) =
  fold@(env0) where
{
//
val+
@COMPENV(rcd) = env0
//
val xs = rcd.hdvarstk
val xs = hdvarstk_fun0(xs)
val () = rcd.hdvarstk := xs
//
} (* end of [xcomp01_dvaradd_fun0] *)
//
implement
xcomp01_dvarpop_fun0
  (env0) =
  fold@(env0) where
{
//
val+
@COMPENV(rcd) = env0
//
val () =
hdvarstk_pop_top(rcd.hdvarstk)
//
val () =
(
  rcd.hdvarstk := xs
) where
{
val-
~hdvarstk_fun0(xs) = rcd.hdvarstk
}
} (* end of [xcomp01_dvarpop_fun0] *)
//
(* ****** ****** *)
//
implement
xcomp01_dvaradd_bind
  (env0, x0, v0) =
  fold@(env0) where
{
//
val+
@COMPENV(rcd) = env0
//
val xs = rcd.hdvarstk
//
val () =
the_dvarmap_insert_one(x0, v0)
//
val () =
rcd.hdvarstk := hdvarstk_cons(x0, xs)
//
} where
{
val v0 = xcomp01_l1valize(env0, v0)
} (* end of [xcomp01_dvaradd_bind] *)
//
(* ****** ****** *)

end // end of [local]

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

in(*in-of-local*)

(* ****** ****** *)

local

typedef
key = hdvar
and
itm = List0(l1val)
vtypedef
dvarmap = map(key, itm)

var
the_dvarmap =
linmap_make_nil<>{key,itm}()
val
the_dvarmap = addr@the_dvarmap

implement
compare_key_key<key>
  (k1, k2) = let
//
val x1 =
$effmask_all(k1.stamp())
and x2 =
$effmask_all(k2.stamp())
//
in $STM.cmp_stamp_stamp(x1, x2) end

(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

implement
the_dvarmap_search_ref
  (k0) = let
//
val
map =
$UN.ptr0_get<dvarmap>(the_dvarmap)
val ref =
linmap_search_ref<key,itm>(map, k0)
//
in
let
prval () = $UN.cast2void(map)
prval () = lemma_p2tr_param(ref) in ref
end
end // end of [the_dvarmap_search_ref]

(* ****** ****** *)

implement
the_dvarmap_insert_one
  (k0, x0) =
let
val p2 =
the_dvarmap_search_ref(k0)
in
if
isneqz(p2)
then
let
  val xs = $UN.p2tr_get(p2)
in
$UN.p2tr_set(p2, list_cons(x0, xs))
end // end of then
else let
  var
  map =
  $UN.ptr0_get<dvarmap>(the_dvarmap)
in
(
$UN.ptr0_set<dvarmap>(the_dvarmap, map)
) where
{
val xs = list_sing(x0)
val () =
linmap_insert_any<key,itm>(map, k0, xs)
}
end // end of [else]
end // end of [the_dvarmap_insert_one]

(* ****** ****** *)

implement
the_dvarmap_remove_one
  ( k0 ) = let
//
var
map =
$UN.ptr0_get<dvarmap>(the_dvarmap)
val
opt =
linmap_takeout_opt<key,itm>(map, k0)
//
in
//
case- opt of
|
~Some_vt(xs) =>
let
val-
list_cons(x0, xs) = xs
in
case+ xs of
|
list_nil _ =>
$UN.ptr0_set<dvarmap>(the_dvarmap, map)
|
list_cons _ =>
(
$UN.ptr0_set<dvarmap>(the_dvarmap, map)
) where
{
val () =
linmap_insert_any<key,itm>(map, k0, xs)
}
end // end of [let]
//
end // end of [the_dvarmap_remove_one]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [xats_xcomp01_envmap.dats] *)
