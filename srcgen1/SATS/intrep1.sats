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
"./../HATS/libxats2cc.hats"
//
(* ****** ****** *)
#staload $INTREP0(*open-it*)
(* ****** ****** *)
//
typedef stamp = $STM.stamp
//
typedef label = $LAB.label
//
typedef loc_t = $LOC.loc_t
//
typedef token = $LEX.token
//
(* ****** ****** *)
typedef g1exp = $S1E.g1exp
(* ****** ****** *)
typedef tyrec = $S2E.tyrec
(* ****** ****** *)
//
sexpdef
  slabeled = $S2E.slabeled
//
(* ****** ****** *)
(*
abstype l1srt_tbox = ptr
typedef l1srt = l1srt_tbox
*)
(* ****** ****** *)
//
abstbox l1tnm_tbox = ptr
typedef l1tnm = l1tnm_tbox
//
typedef
l1tnmlst = List0(l1tnm)
typedef
l1tnmopt = Option(l1tnm)
//
vtypedef
l1tnmlst_vt = List0_vt(l1tnm)
//
(* ****** ****** *)
//
datatype
l1ctp =
//
|
L1CTPnone of ()
|
L1CTPsome of ()
|
L1CTPname of string
|
L1CTPltnm of (l1tnm)
|
L1CTPtype of (h0typ)
|
L1CTPtyrec of
(int(*bxty*), labl1ctplst)
|
L1CTPtydat of
(htcst, h0typlst, l1dtclst)
|
L1CTPtyapp of (l1ctp, l1ctplst)
//
and l1dtc =
L1DTCdtcon of (h0con, l1ctplst)
//
where
labl1ctp = slabeled(l1ctp)
and l1ctplst = List0(l1ctp)
and l1dtclst = List0(l1dtc)
and labl1ctplst = List0(labl1ctp)
//
(* ****** ****** *)
//
abstype l1exn_tbox = ptr
typedef l1exn = l1exn_tbox
//
(* ****** ****** *)
//
abstype l1tmp_tbox = ptr
typedef l1tmp = l1tmp_tbox
//
typedef l1tmplst = List0(l1tmp)
typedef l1tmpopt = Option(l1tmp)
//
(* ****** ****** *)
//
(*
HX-2020-09-06:
for template instances
*)
//
abstype l1cst_tbox = ptr
typedef l1cst = l1cst_tbox
typedef l1cstlst = List0(l1cst)
//
(* ****** ****** *)
//
abstype l1val_tbox = ptr
typedef l1val = l1val_tbox
//
(* ****** ****** *)
//
typedef
l1valist = List0(l1val)
//
typedef
l1valopt = Option(l1val)
vtypedef
l1valopt_vt = Option_vt(l1val)
//
(* ****** ****** *)
//
abstype l1cmd_tbox = ptr
typedef l1cmd = l1cmd_tbox
//
typedef l1cmdlst = List0(l1cmd)
typedef l1cmdopt = Option(l1cmd)
//
(* ****** ****** *)
//
(*
abstype l1blk_tbox = ptr
typedef l1blk = l1blk_tbox
*)
//
datatype
l1blk =
| L1BLKnone of ()
| L1BLKsome of (l1cmdlst)
//
typedef l1blklst = List0(l1blk)
//
(* ****** ****** *)
//
abstype l1dcl_tbox = ptr
typedef l1dcl = l1dcl_tbox
//
typedef l1dclist = List0(l1dcl)
typedef l1dclopt = Option(l1dcl)
//
(* ****** ****** *)
//
fun
h0typ_compare
(h0typ, h0typ): int(*sign*)
//
#symload
compare with h0typ_compare
//
(* ****** ****** *)
//
fun
print_l1ctp: print_type(l1ctp)
fun
prerr_l1ctp: prerr_type(l1ctp)
fun
fprint_l1ctp: fprint_type(l1ctp)
//
overload print with print_l1ctp
overload prerr with prerr_l1ctp
overload fprint with fprint_l1ctp
//
fun
fprint_labl1ctp
(out: FILEref, lx0: labl1ctp): void
//
(* ****** ****** *)
//
fun
print_l1dtc: print_type(l1dtc)
fun
prerr_l1dtc: prerr_type(l1dtc)
fun
fprint_l1dtc: fprint_type(l1dtc)
//
overload print with print_l1dtc
overload prerr with prerr_l1dtc
overload fprint with fprint_l1dtc
//
(* ****** ****** *)
//
fun
l1tnm_stamp_new(): stamp
//
fun
l1tnm_get_htsz(l1tnm): (int)
//
fun
l1tnm_get_type(l1tnm): h0typ
fun
l1tnm_get_lctp(l1tnm): l1ctp
fun
l1tnm_get_stamp(l1tnm): stamp
//
#symload .htsz with l1tnm_get_htsz
#symload .type with l1tnm_get_type
#symload .lctp with l1tnm_get_lctp
#symload .stamp with l1tnm_get_stamp
//
fun
l1tnm_set_lctp(l1tnm,l1ctp): void
//
#symload .lctp with l1tnm_set_lctp
//
(* ****** ****** *)
//
fun
l1tnm_none0(): l1tnm
fun
l1tnm_make_type(h0typ): l1tnm
//
#symload .type with l1tnm_get_type
#symload .stamp with l1tnm_get_stamp
//
(* ****** ****** *)
//
fun
l1tnm_compare
(l1tnm, l1tnm): int(*sign*)
//
#symload
compare with l1tnm_compare
//
(* ****** ****** *)
fun
the_ltnmmap_listize
  ( (*void*) ): l1tnmlst_vt
(* ****** ****** *)
//
fun
the_ltnmmap_search_opt
(h0t0: h0typ): Option_vt(l1tnm)
//
fun
the_ltnmmap_insert_exn
(h0t0: h0typ, ltnm: l1tnm): void
//
(* ****** ****** *)
//
fun
h0typ_tnmize(h0typ): l1tnm
fun
h0typ_tnmize_rec(h0typ): l1tnm
//
(* ****** ****** *)
//
fun
print_l1tnm: print_type(l1tnm)
fun
prerr_l1tnm: prerr_type(l1tnm)
fun
fprint_l1tnm: fprint_type(l1tnm)
//
overload print with print_l1tnm
overload prerr with prerr_l1tnm
overload fprint with fprint_l1tnm
//
(* ****** ****** *)
//
fun
l1tnm_ctpize(l1tnm): l1ctp
fun
h0typ_ctpize_rec(h0typ): l1ctp
//
fun
the_ltnmmap_ctpize((*void*)): void
//
(* ****** ****** *)
//
fun
l1exn_new_loc
(loc: loc_t): l1exn
fun
l1exn_stamp_new(): stamp
//
fun
l1exn_get_loc(l1exn): loc_t
overload .loc with l1exn_get_loc
fun
l1exn_get_stamp(l1exn): stamp
overload .stamp with l1exn_get_stamp
//
(* ****** ****** *)
//
fun
print_l1exn: print_type(l1exn)
fun
prerr_l1exn: prerr_type(l1exn)
fun
fprint_l1exn: fprint_type(l1exn)
//
overload print with print_l1exn
overload prerr with prerr_l1exn
overload fprint with fprint_l1exn
//
(* ****** ****** *)
//
fun
l1tmp_new_tmp
(loc: loc_t): l1tmp
fun
l1tmp_new_arg
(loc: loc_t, idx: int): l1tmp
//
fun
l1tmp_get_loc(l1tmp): loc_t
overload .loc with l1tmp_get_loc
fun
l1tmp_get_arg(tmp: l1tmp): int
overload .arg with l1tmp_get_arg
fun
l1tmp_get_ref(tmp: l1tmp): int
overload .ref with l1tmp_get_ref
//
(* ****** ****** *)
fun
l1tmp_get_lev(tmp: l1tmp): int
overload .lev with l1tmp_get_lev
fun
l1tmp_set_lev(l1tmp, int): void
overload .lev with l1tmp_set_lev
(* ****** ****** *)
//
fun
l1tmp_get_ltnm(tmp: l1tmp): l1tnm
fun
l1tmp_set_ltnm
(tmp1: l1tmp, l1t2: l1tnm) : void
//
overload .ltnm with l1tmp_get_ltnm
overload .ltnm with l1tmp_set_ltnm
//
(* ****** ****** *)
fun
l1tmp_stamp_new(): stamp
fun
l1tmp_get_stamp(l1tmp): stamp
overload .stamp with l1tmp_get_stamp
(* ****** ****** *)
fun
eq_l1tmp_l1tmp(l1tmp, l1tmp): bool
(* ****** ****** *)
//
fun
print_l1tmp: print_type(l1tmp)
fun
prerr_l1tmp: prerr_type(l1tmp)
fun
fprint_l1tmp: fprint_type(l1tmp)
//
overload print with print_l1tmp
overload prerr with prerr_l1tmp
overload fprint with fprint_l1tmp
//
(* ****** ****** *)
//
fun
l1cst_new_hdc
(loc: loc_t, hdc: h0cst): l1cst
//
(* ****** ****** *)
fun
l1cst_get_loc(l1cst): loc_t
overload .loc with l1cst_get_loc
fun
l1cst_get_hdc(l1cst): h0cst
overload .hdc with l1cst_get_hdc
(* ****** ****** *)
fun
l1cst_stamp_new((*void*)): stamp
fun
l1cst_get_stamp(l1cst): stamp
overload .stamp with l1cst_get_stamp
(* ****** ****** *)
//
fun
print_l1cst: print_type(l1cst)
fun
prerr_l1cst: prerr_type(l1cst)
fun
fprint_l1cst: fprint_type(l1cst)
//
overload print with print_l1cst
overload prerr with prerr_l1cst
overload fprint with fprint_l1cst
//
(* ****** ****** *)
//
datatype l1con =
| L1CONcon of h0con // non-ext
| L1CONval of l1val // ext-con
//
(* ****** ****** *)
//
fun
print_l1con: print_type(l1con)
fun
prerr_l1con: prerr_type(l1con)
fun
fprint_l1con: fprint_type(l1con)
//
overload print with print_l1con
overload prerr with prerr_l1con
overload fprint with fprint_l1con
//
(* ****** ****** *)
//
datatype
l1pck =
| L1PCKany of ()
(*
| L1PCKint of (int, l1val)
| L1PCKbtf of (bool, l1val)
*)
//
| L1PCKi00 of
  ( int, l1val )
| L1PCKb00 of
  ( bool, l1val )
| L1PCKs00 of
  (string, l1val)
//
| L1PCKint of
  ( token, l1val )
| L1PCKbtf of
  ( token, l1val )
| L1PCKchr of
  ( token, l1val )
| L1PCKstr of
  ( token, l1val )
//
| L1PCKcon of
  ( l1con, l1val )
//
| L1PCKapp of
  ( l1pck(*con-tag*)
  , l1pcklst(*con-arg*) )
//
| L1PCKtup of
  ( int//(*tup-knd*)
  , l1pcklst(*tup-arg*) )
//
| L1PCKgpat of
  ( l1pck(*gpat-pat*)
  , l1pcklst(*gpat-gua*) )
| L1PCKgexp of (l1val, l1blk)
| L1PCKgmat of (h0exp, h0pat)
//
//
| L1PCKxpat of (h0pat, l1val)
//
where l1pcklst = List0(l1pck)
//
fun
print_l1pck: print_type(l1pck)
fun
prerr_l1pck: prerr_type(l1pck)
fun
fprint_l1pck: fprint_type(l1pck)
//
overload print with print_l1pck
overload prerr with prerr_l1pck
overload fprint with fprint_l1pck
//
(* ****** ****** *)
//
typedef
lvnam = string
//
datatype
l1val_node =
//
| L1VALi00 of (int)
| L1VALb00 of (bool)
| L1VALs00 of string
//
| L1VALint of (token)
| L1VALbtf of (token)
| L1VALchr of (token)
//
| L1VALflt of (token)
| L1VALstr of (token)
//
| L1VALtop of (token)
//
| L1VALnam of (lvnam)
//
| L1VALexn of (l1exn)
| L1VALtmp of (l1tmp)
//
| L1VALcon of (l1con)
//
| L1VALcfun of (h0cst)
//
| L1VALctmp of
  (l1cst, l1dcl(*def*))
//
| L1VALvfix of (h0var)
//
| L1VALaddr of (l1val)
| L1VALflat of (l1val)
| L1VALtalf of (l1val)
//
// For boxed
// HX: ctag: con_tag
// HX: carg: con_arg
//
| L1VALctag of (l1val)
| L1VALcarg of (l1val, int(*idx*))
| L1VALcofs of (l1val, int(*idx*))
//
// For flat (tuples)
//
| L1VALtarg of (l1val, int(*idx*))
| L1VALtptr of (l1val, int(*idx*))
//
| L1VALnone0 of () | L1VALnone1 of (h0exp)
//
(* ****** ****** *)
//
fun
l1val_make_node
(loc_t, l1val_node): l1val
//
(* ****** ****** *)
//
fun
l1val_get_loc
(l1v0: l1val): loc_t
fun
l1val_get_node
(l1v0: l1val): l1val_node
//
overload .loc with l1val_get_loc
overload .node with l1val_get_node
//
(* ****** ****** *)
//
fun
print_l1val: print_type(l1val)
fun
prerr_l1val: prerr_type(l1val)
fun
fprint_l1val: fprint_type(l1val)
//
overload print with print_l1val
overload prerr with prerr_l1val
overload fprint with fprint_l1val
//
(* ****** ****** *)
//
fun
l1val_exn(exn: l1exn): l1val
//
fun
l1val_tmp(tmp: l1tmp): l1val
fun
l1val_tmp2
(loc:loc_t, tmp:l1tmp): l1val
//
(* ****** ****** *)
//
fun
l1val_flat(l1v0: l1val): l1val
//
fun
l1val_addr(l1v0: l1val): l1val
fun
l1val_talf(l1v0: l1val): l1val
//
(* ****** ****** *)
fun
l1val_ctag
( loc0: loc_t
, l1v0: l1val): l1val
fun
l1val_carg
( loc0: loc_t
, l1v1: l1val, idx2: int): l1val
fun
l1val_cofs
( loc0: loc_t
, l1v1: l1val, idx2: int): l1val
(* ****** ****** *)
fun
l1val_targ
( loc0: loc_t
, l1v1: l1val, idx2: int): l1val
fun
l1val_tptr
( loc0: loc_t
, l1v1: l1val, idx2: int): l1val
(* ****** ****** *)
fun
l1val_eval
( knd0: int, l1v1: l1val ): l1val
(* ****** ****** *)
fun
l1val_free
( knd0: int, l1v1: l1val ): l1val
(* ****** ****** *)
//
fun
l1val_addrize(l1v0: l1val): l1val
fun
l1val_talfize(l1v0: l1val): l1val
//
(* ****** ****** *)
//
datatype
l1cmd_node =
//
| L1CMDmov of
  (l1tmp, l1val)
//
| L1CMDcon of
  ( l1tmp(*res*)
  , h0con(*con*)
  , l1valist(*arg*))
//
// HX: 0: flat
// HX: 1: boxed
| L1CMDtup of
  ( l1tmp(*res*)
  , int // flt/box
  , l1valist(*arg*))
//
| L1CMDapp of
  ( l1tmp(*res*)
  , l1val(*fun*)
  , l1valist(*arg*))
//
(*
| L1CMDlam of
  ( l1tmp(*res*)
  , l1lamexp(*fun*))
| L1CMDfix of
  ( l1tmp(*res*)
  , l1fixexp(*fun*))
*)
//
| L1CMDlazy of
  ( l1tmp(*res*)
  , l1val(*thunk*) )
| L1CMDllazy of
  ( l1tmp(*res*)
  , l1val(*thunk*)
  , l1val(*frees*) )
//
| L1CMDblk of (l1blk)
| L1CMDdcl of (l1dcl)
//
| L1CMDift1 of
  (l1val, l1blk, l1blk)
//
| L1CMDcase of
  ( int(*knd*)
  , l1val
  , l1tmp(*tcas*)
  , l1pcklst, l1blklst)
//
| L1CMDtry0 of
  ( l1blk(*try*)
  , l1exn(*texn*)
  , l1tmp(*tcas*)
  , l1pcklst, l1blklst)
//
| L1CMDpatck of (l1pck)
| L1CMDmatch of (h0pat, l1val)
//
| L1CMDflat of
  (l1tmp(*res*), l1val(*lval*))
//
| L1CMDcarg of
  ( l1tmp(*res*)
  , l1val(*lval*), int(*index*))
| L1CMDcofs of
  ( l1tmp(*res*)
  , l1val(*lval*), int(*index*))
| L1CMDtarg of
  ( l1tmp(*res*)
  , l1val(*lval*), int(*index*))
| L1CMDtofs of
  ( l1tmp(*res*)
  , l1val(*lval*), int(*index*))
//
| L1CMDexcon of l1tmp(*exc-tag*)
| L1CMDraise of l1val(*lin-exn*)
//
| L1CMDassgn of // assignment
  (l1val(*lval*), l1val(*rval*))
//
| L1CMDeval0 of // unknown
  (l1tmp(*res*), l1val(*source*))
| L1CMDeval1 of // ptr-dref
  (l1tmp(*res*), l1val(*source*))
| L1CMDeval2 of // lazy-eval
  (l1tmp(*res*), l1val(*source*))
| L1CMDeval3 of // llazy-eval
  (l1tmp(*res*), l1val(*source*))
//
| L1CMDfree0 of // unknown
  (l1tmp(*res*), l1val(*source*))
| L1CMDfree1 of // ptr-free
  (l1tmp(*res*), l1val(*source*))
| L1CMDfree2 of // con-free
  (l1tmp(*res*), l1val(*source*))
| L1CMDfree3 of // llazy-free
  (l1tmp(*res*), l1val(*source*))
//
(* ****** ****** *)
//
fun
l1cmd_make_node
(loc_t, l1cmd_node): l1cmd
//
(* ****** ****** *)
//
fun
l1cmd_get_loc
(lcmd: l1cmd): loc_t
fun
l1cmd_get_node
(lcmd: l1cmd): l1cmd_node
//
overload .loc with l1cmd_get_loc
overload .node with l1cmd_get_node
//
(* ****** ****** *)
//
fun
print_l1cmd: print_type(l1cmd)
fun
prerr_l1cmd: prerr_type(l1cmd)
fun
fprint_l1cmd: fprint_type(l1cmd)
//
overload print with print_l1cmd
overload prerr with prerr_l1cmd
overload fprint with fprint_l1cmd
//
(* ****** ****** *)
//
fun
l1blk_none(): l1blk
fun
l1blk_some(cmds: l1cmdlst): l1blk
//
(* ****** ****** *)
//
fun
print_l1blk: print_type(l1blk)
fun
prerr_l1blk: prerr_type(l1blk)
fun
fprint_l1blk: fprint_type(l1blk)
//
overload print with print_l1blk
overload prerr with prerr_l1blk
overload fprint with fprint_l1blk
//
(* ****** ****** *)
//
datatype
l1pkg =
L1PKG of (l1tmplst, l1dclist)
//
typedef filpath = $FP0.filpath
//
(* ****** ****** *)
//
datatype
l1fundecl =
L1FUNDECL of
@{
  loc= loc_t
, nam= h0var
, hdc= h0cst
//
, hfg=
  h0faglstopt
//
, lfg=
  l1faglstopt
//
, def= l1valopt
, rtp= l1tnmopt
//
, lev= int//fun
, lts= l1tmplst
//
, lfg_blk= l1blk
, def_blk= l1blk
} where
l1fag=l1tmplst
and
l1faglst=List0(l1fag)
and
l1faglstopt=Option(l1faglst)
(* end of [L1FUNDECL] *)
//
typedef
l1fundeclist = List0(l1fundecl)
//
(* ****** ****** *)
//
fun
print_l1fundecl:
print_type(l1fundecl)
fun
prerr_l1fundecl:
prerr_type(l1fundecl)
fun
fprint_l1fundecl:
fprint_type(l1fundecl)
//
overload print with print_l1fundecl
overload prerr with prerr_l1fundecl
overload fprint with fprint_l1fundecl
//
(* ****** ****** *)
//
datatype
l1valdecl =
L1VALDECL of @{
  loc= loc_t
, pat= h0pat
, def= l1valopt
, def_blk= l1blk
} (* end of [L1VALDECL] *)
//
typedef
l1valdeclist = List0(l1valdecl)
//
(* ****** ****** *)
//
fun
print_l1valdecl:
print_type(l1valdecl)
fun
prerr_l1valdecl:
prerr_type(l1valdecl)
fun
fprint_l1valdecl:
fprint_type(l1valdecl)
//
overload print with print_l1valdecl
overload prerr with prerr_l1valdecl
overload fprint with fprint_l1valdecl
//
(* ****** ****** *)
//
datatype
l1vardecl =
L1VARDECL of @{
  loc= loc_t
(*
, hdv= hdvar
, ini= l1valopt
, hdv_tmp= l1tmp
*)
(*
, ini_blk= l1blk
*)
}
//
typedef
l1vardeclist = List0(l1vardecl)
//
(* ****** ****** *)
//
fun
print_l1vardecl:
print_type(l1vardecl)
fun
prerr_l1vardecl:
prerr_type(l1vardecl)
fun
fprint_l1vardecl:
fprint_type(l1vardecl)
//
overload print with print_l1vardecl
overload prerr with prerr_l1vardecl
overload fprint with fprint_l1vardecl
//
(* ****** ****** *)
//
datatype
l1implmnt3 =
L1IMPLMNT3 of @{
  loc= loc_t
//
, hdc= h0cst
//
, hfg= h0faglst
, lfg= l1faglst
//
, def= l1valopt
//
, lev= int//fun
, lts= l1tmplst
//
, lfg_blk= l1blk
, def_blk= l1blk
}
//
(* ****** ****** *)
//
fun
print_l1implmnt3:
print_type(l1implmnt3)
fun
prerr_l1implmnt3:
prerr_type(l1implmnt3)
fun
fprint_l1implmnt3:
fprint_type(l1implmnt3)
//
overload print with print_l1implmnt3
overload prerr with prerr_l1implmnt3
overload fprint with fprint_l1implmnt3
//
(* ****** ****** *)
//
datatype
l1dcl_node =
//
|
L1DCLfundclst of
( token
, decmodopt, l1fundeclist)
//
|
L1DCLvaldclst of
( token
, decmodopt, l1valdeclist)
|
L1DCLvardclst of
( token
, decmodopt, l1vardeclist)
//
|
L1DCLexcptcon of (h0conlst)
|
L1DCLdatatype of (htcstlst)
//
|
L1DCLimplmnt3 of
( token
, decmodopt, l1implmnt3(*single*))
//
|
L1DCLtimpcst3 of ( l1cst, l1dcl )
//
| L1DCLnone0 of () | L1DCLnone1 of h0dcl
//
where decmodopt = $D0E.decmodopt
  and filpathopt = Option(filpath)
  and l1dclistopt = Option(l1dclist)
//
(* ****** ****** *)
//
fun
l1dcl_make_node
(loc_t, l1dcl_node): l1dcl
//
(* ****** ****** *)
//
fun
l1dcl_get_loc
(ldcl: l1dcl): loc_t
fun
l1dcl_get_node
(ldcl: l1dcl): l1dcl_node
//
overload .loc with l1dcl_get_loc
overload .node with l1dcl_get_node
//
(* ****** ****** *)
//
fun
print_l1dcl: print_type(l1dcl)
fun
prerr_l1dcl: prerr_type(l1dcl)
fun
fprint_l1dcl: fprint_type(l1dcl)
//
overload print with print_l1dcl
overload prerr with prerr_l1dcl
overload fprint with fprint_l1dcl
//
(* ****** ****** *)
//
fun
xemit01_l1tnm
(FILEref, l1t: l1tnm): void
//
(* ****** ****** *)
//
fun
xemit01_htcst
(FILEref, htc: htcst): void
//
(* ****** ****** *)
//
fun
xemit01_htdat1
(FILEref, htc: htcst): void
fun
xemit01_htdat2
(FILEref, htc: htcst): void
//
(* ****** ****** *)
//
fun
xemit01_int00
(out: FILEref, int: int): void
fun
xemit01_btf00
(out: FILEref, btf: bool): void
//
(* ****** ****** *)
//
fun
xemit01_txt00
(out: FILEref, txt: string): void
fun
xemit01_txtln
(out: FILEref, txt: string): void
//
(* ****** ****** *)
fun
xemit01_newln(FILEref): void
(* ****** ****** *)
fun
xemit01_h0var(FILEref, h0var): void
(* ****** ****** *)
fun
xemit01_h0con(FILEref, h0con): void
fun
xemit01_h0cst(FILEref, h0cst): void
(* ****** ****** *)
fun
xemit01_l1con(FILEref, l1con): void
(* ****** ****** *)
fun
xemit01_l1cst(FILEref, l1cst): void
(* ****** ****** *)
//
fun
xemit01_lvi00(FILEref, int): void
fun
xemit01_lvb00(FILEref, bool): void
//
(* ****** ****** *)
//
fun
xemit01_lvint(FILEref, token): void
fun
xemit01_lvbtf(FILEref, token): void
fun
xemit01_lvchr(FILEref, token): void
//
(* ****** ****** *)
//
fun
xemit01_lvflt(FILEref, token): void
fun
xemit01_lvstr(FILEref, token): void
//
(* ****** ****** *)
fun
xemit01_lvtop(FILEref, token): void
(* ****** ****** *)
fun
xemit01_lvnam(FILEref, lvnam): void
(* ****** ****** *)
fun
xemit01_l1exn(FILEref, l1exn): void
fun
xemit01_l1tmp(FILEref, l1tmp): void
(* ****** ****** *)
fun
xemit01_l1val(FILEref, l1val): void
(* ****** ****** *)
fun
xemit01_l1pck(FILEref, l1pck): void
(* ****** ****** *)
fun
xemit01_l1cmd(FILEref, l1cmd): void
fun
xemit01_l1cmdlst
(out:FILEref, lcmds:l1cmdlst): void
(* ****** ****** *)
fun
xemit01_l1blk(FILEref, l1blk): void
(* ****** ****** *)

fun
xemit01_l0fag
(out: FILEref, lfa0: l1fag ): void
fun
xemit01_l0faglst
(out: FILEref, lfas: l1faglst): void

(* ****** ****** *)
fun
xemit01_fargdecs
( out: FILEref
, narg: int, flev: int): void
fun
xemit01_ftmpdecs
(out: FILEref, tmps: l1tmplst): void
(* ****** ****** *)
fun
xemit01_l1dcl
(out: FILEref, dl0: l1dcl): void
(* ****** ****** *)
fun
xemit01_l1dcl_fun
(out: FILEref, dcl0: l1dcl): void
fun
xemit01_l1dcl_fnx
(out: FILEref, dcl0: l1dcl): void
(* ****** ****** *)
//
fun
xemit01_package(FILEref, l1pkg): void
//
(* ****** ****** *)

(* end of [xats_intrep1.sats] *)
