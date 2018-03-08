(** Generic in-memory map *)

open Mem_store
open Store_to_map
open Params
open Map_ops

let mk_map_ops ~ps ~ops : ('k,'v,'t) map_ops = 
  Mem_store.mk_store_ops (mem_ops ops) |> fun store_ops ->
  store_ops_to_map_ops ~ps ~page_ref_ops:(page_ref_ops ops) ~store_ops
