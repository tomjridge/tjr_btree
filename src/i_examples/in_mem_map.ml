(* in mem map ---------------------------------------- *)

open Prelude
open Btree_api

open In_mem_store
module S2M = Store_to_map

open Isa_util.Params_

let mk_unchecked_map_ops ps0 im_ops pr_ops : ('k,'v,'t) map_ops = (
  let s = In_mem_store.make im_ops ps0.constants in
  let (store_read,store_free,store_alloc) = (s.store_read,s.store_free,s.store_alloc) in
  let ps1 = Isa_util.Params_.({ ps0; store_read; store_free; store_alloc }) in
  let map_ops = S2M.make_unchecked_map_ops ps1 pr_ops in
  map_ops
)
