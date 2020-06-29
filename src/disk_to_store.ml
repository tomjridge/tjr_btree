(** Use pickling to convert a disk-like thing to a store-like thing.

Convert a disk to a store using pickling and a freespace allocator for
   disk blocks; require page size and block size are the same.  *)

open Btree_intf

let disk_to_store 
    ~(monad_ops:'t monad_ops) ~(disk_ops:('r,'t,'dnode,'blk)disk_ops) 
  = 
  let { dnode_mshlr=marshal; blk_dev_ops; blk_alloc } = disk_ops in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { blk_sz; read; write; write_many=_ } = blk_dev_ops in
  let { blk_sz=blk_sz'; dnode_to_blk; blk_to_dnode } = marshal in
  Test.check(fun _ -> assert (blk_sz = blk_sz'));
  let ops : (_,_,_) store_ops = {
    read=(fun r ->
        read ~blk_id:r >>= fun blk ->
        blk |> blk_to_dnode |> return);
    wrte=(fun dn ->
        blk_alloc.blk_alloc () >>= fun blk_id -> 
        dn |> dnode_to_blk |> fun blk -> 
        write ~blk_id ~blk >>= fun () -> 
        return blk_id);
    rewrite=(fun blk_id dn ->
        dn |> dnode_to_blk |> fun blk -> 
        write ~blk_id ~blk >>= fun () -> 
        return None); (* NOTE this always rewrites FIXME *)
    free=(fun _rs -> return ())}    
  in
  ops

let _ = disk_to_store
