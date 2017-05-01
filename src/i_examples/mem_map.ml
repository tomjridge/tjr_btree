(* in mem map ---------------------------------------- *)

open Prelude
open Btree_api

open Mem_store

let mk_map_ops ps mem_ops pr_ops : ('k,'v,'t) map_ops = 
  let store_ops = Mem_store.mk_store_ops mem_ops in
  let (constants,compare_k,debug) = (constants ps, compare_k ps,debug ps) in
  let ps = 
    object
      method constants=constants
      method compare_k=compare_k;
      method debug=debug;
      method store_ops=store_ops;
    end
  in            
  Store_to_map.make_map_ops ps pr_ops
