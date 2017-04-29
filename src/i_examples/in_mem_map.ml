(* in mem map ---------------------------------------- *)

open Prelude
open Btree_api

open In_mem_store
open Isa_util_params
open Isa_util.Export

module S2M = Store_to_map

let mk_unchecked_map_ops ps0 im_ops pr_ops : ('k,'v,'t) map_ops = (
  let store_ops = In_mem_store.make im_ops in
  let ps1 = { ps0; store_ops } in
  let map_ops = S2M.make_unchecked_map_ops ps1 pr_ops in
  map_ops
)

let mk_checked_map_ops ps0 r2t im_ops pr_ops : ('k,'v,'t) map_ops = (
  let store_ops = In_mem_store.make im_ops in
  let ps1 : ('k,'v,'r,'t)ps1 = { ps0; store_ops } in
  let map_ops = S2M.make_checked_map_ops ps1 r2t pr_ops in
  map_ops
)

