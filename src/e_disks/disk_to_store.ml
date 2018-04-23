open Base_types
open Page_ref_int (* FIXME generalize *)
open Params
open Disk_ops

(** We maintain a free counter in the global state in order to
    allocate new blocks *)
type 't free_ops = (int,'t) mref


(** Use pickling to convert a disk-like thing to a store-like thing.

Convert a disk to a store using pickling and a free counter; assume
page size and block size are the same.
 *)
let disk_to_store ~ps ~disk_ops ~free_ops =
  dest_disk_ops disk_ops @@ fun ~blk_sz ~read ~write ->
  let page_size = page_size ps in
  let f2p = frame_to_page ps page_size in
  let p2f = page_to_frame ps in
  Test.test(fun _ -> assert (blk_sz = page_size));
  let store_free rs = return () in  (* no-op *)
  let store_alloc f : (page_ref,'t) m = 
    f |> f2p |> fun p -> 
    free_ops.get () |> bind @@ fun free -> 
    write free p |> bind @@ fun () -> 
    free_ops.set (free+1) |> bind @@ fun () ->
    return free
  in
  let store_read r : (('k,'v)frame,'t) m = 
    read r |> bind @@ fun blk ->
    blk |> p2f |> return
  in
  Store_ops.mk_store_ops ~store_free ~store_read ~store_alloc

