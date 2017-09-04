(** Generic in-memory map *)

open Prelude
open Btree_api

open Mem_store
open Store_to_map
open Params

let mk_map_ops ~ps ~ops : [<`Map_ops of 'a] = 
  Mem_store.mk_store_ops (mem_ops ops) |> fun store_ops ->
  store_ops_to_map_ops ~ps ~page_ref_ops:(page_ref_ops ops) ~store_ops
