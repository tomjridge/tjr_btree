(* construct various layers based on parameters ps *)
open Params
open Map_on_fd
open Map_on_fd.Default_implementation

let mk_example ~ps = (
  let disk_ops = mk_disk_ops ~ps ~fd_ops in
  let store_ops = Disk_to_store.disk_to_store ~ps ~disk_ops ~free_ops in
  let map_ops = 
    Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops in
  let imperative_map_ops = Btree_api.Imperative_map_ops.of_map_ops map_ops in
  let ls_ops = mk_ls_ops ~ps ~page_ref_ops ~store_ops in
  let from_file ~fn ~create ~init = from_file ~fn ~create ~init ~ps in
  let close = close ~blk_sz:(blk_sz ps) in
  object
    method disk_ops=disk_ops
    method store_ops=store_ops
    method map_ops=map_ops
    method imperative_map_ops=imperative_map_ops
    method ls_ops=ls_ops
    method from_file=from_file
    method close=close
  end
)

let from_file x = x#from_file
let close x = x#close
let map_ops x = x#map_ops
let imperative_map_ops x = x#imperative_map_ops
let ls_ops x = x#ls_ops

let blk_sz = 4096

