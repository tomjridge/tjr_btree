open Tjr_fs_shared.Blk_dev_ops_type
open Base_types
open Page_ref_int (* FIXME generalize *)
open Marshalling_params_type
open Freespace_ops_type

(** Use pickling to convert a disk-like thing to a store-like thing.

Convert a disk to a store using pickling and a freespace allocator for
   disk blocks; require page size and block size are the same.  *)
let disk_to_store ~monad_ops ~ps ~disk_ops ~free_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { blk_sz; read; write } = disk_ops in
  let page_size = ps.page_size in
  Test.test(fun _ -> assert (blk_sz = page_size));
  let f2p = ps.frame_to_page in
  let p2f = ps.page_to_frame in
  let store_free _rs = return () in  (* no-op *)
  let store_alloc f : (page_ref,'t) m = 
    f |> f2p |> fun p -> 
    free_ops.alloc () >>= fun free -> 
    write ~blk_id:free ~blk:p >>= fun () -> 
    return free
  in
  let store_read r : (('k,'v)frame,'t) m = 
    read ~blk_id:r >>= fun blk ->
    blk |> p2f |> return
  in
  Store_ops.mk_store_ops ~store_free ~store_read ~store_alloc

let _ = disk_to_store
