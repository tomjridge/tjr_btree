open Params
open Map_on_fd
open Map_on_fd.Default_implementation

(** Construct various api layers based on parameters ps *)
let mk_example_on_fd ~ps = 
  let constants = constants ps in
  let cmp = cmp ps in
  let disk_ops = mk_disk_ops ~monad_ops ~ps ~fd_ops in
  let store_ops = Disk_to_store.disk_to_store ~monad_ops ~ps ~disk_ops ~free_ops in
  let map_ops = Store_to_map.store_ops_to_map_ops 
      ~monad_ops ~constants ~cmp ~page_ref_ops ~store_ops 
  in
  let ls_ops = 
    Store_to_map.store_ops_to_ls_ops ~monad_ops ~constants ~cmp ~store_ops 
  in
  let from_file ~fn ~create ~init = from_file ~fn ~create ~init ~ps in
  let close = close ~blk_sz:(blk_sz ps) in
  object
    method disk_ops=disk_ops
    method store_ops=store_ops
    method map_ops=map_ops
    method ls_ops=ls_ops
    method from_file=from_file
    method close=close
  end


(* specific params ------------------------------------------------ *)

let map_ops x = x#map_ops
let ls_ops x = x#ls_ops
let from_file x = x#from_file
let close x = x#close


(* default blocksize ------------------------------------------------ *)

let blk_sz = 4096


