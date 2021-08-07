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

datavtype
compenv =
COMPENV of @{
  flevel= int
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
} (*where*) // end of [compenv_free_top]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [xats_xcomp01_envmap.dats] *)
