(* construct various layers based on parameters ps *)
open Params
open Map_on_fd
open Map_on_fd.Default_implementation


let mk_example ~ps ~kk = (
  let disk_ops = mk_disk_ops ~ps ~fd_ops in
  let store_ops = Disk_to_store.disk_to_store_with_custom_marshalling
      ~ps ~disk_ops ~free_ops
  in
  let map_ops = 
    Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops in
  let imperative_map_ops = Btree_api.Imperative_map_ops.of_map_ops map_ops in
  let ls_ops = mk_ls_ops ~ps ~page_ref_ops ~store_ops in
  let from_file ~fn ~create ~init = from_file ~fn ~create ~init ~ps in
  let close = close ~blk_sz:(blk_sz ps) in
  kk ~disk_ops ~store_ops ~map_ops ~imperative_map_ops ~ls_ops ~from_file ~close
)
