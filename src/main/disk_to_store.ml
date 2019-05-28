(* open Tjr_monad.Types *)
(* open Tjr_fs_shared *)
(* open Isa_btree *)
(* open Isa_export_wrapper *)
open Marshalling_ops_type  (* FIXME include in base_types? *)
open Blk_allocator_ops_type  (* FIXME include in base_types? *)

(** UNCACHED Use pickling to convert a disk-like thing to a store-like thing.

Convert a disk to a store using pickling and a freespace allocator for
   disk blocks; require page size and block size are the same.  *)
let uncached_disk_to_store ~monad_ops ~marshalling_ops:marshal
    ~blk_dev_ops ~blk_allocator_ops:alloc =
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

let _ = uncached_disk_to_store

(** Prettier type (essentially [blk_dev*marshalling->store]): {%html:<pre>
monad_ops:'a monad_ops ->
marshalling_ops:('b, 'c) marshalling_ops ->
blk_dev_ops:('d, 'c, 'a) Blk_dev_ops_type.blk_dev_ops ->
blk_allocator_ops:('d, 'a) blk_allocator_ops -> 
('d, 'b, 'a) store_ops
</pre> %}
*)

(* let disk_to_store = uncached_disk_to_store *)
