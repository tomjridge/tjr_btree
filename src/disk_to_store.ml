(** Use pickling to convert a disk-like thing to a store-like thing.

Convert a disk to a store using pickling and a freespace allocator for
   disk blocks; require page size and block size are the same.  *)

open Btree_intf

let disk_to_store ~monad_ops ~disk_ops = 
  let { marshalling_ops=marshal; blk_dev_ops; blk_allocator_ops=alloc } = disk_ops in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { blk_sz; read; write } = blk_dev_ops in
  let { marshal_blk_size; dnode_to_blk; blk_to_dnode } = marshal in
  Test.test(fun _ -> assert (Blk_sz.to_int blk_sz = marshal_blk_size));
  let ops = {
    read=(fun r ->
        read ~blk_id:r >>= fun blk ->
        blk |> blk_to_dnode |> return);
    wrte=(fun dn ->
        alloc.alloc () >>= fun blk_id -> 
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