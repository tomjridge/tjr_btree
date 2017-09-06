(** Store operations: alloc, free and read. The store is a level above
   the raw disk block device, on which we build the B-tree. *)

open Frame
open Monad


(* store ------------------------------------------------------------ *)

let wf_store_ops
    ~(store_free: 'r list -> (unit,'t) m)
    ~(store_read: 'r -> (('k, 'v,'r) frame,'t) m)  (* FIXME option? *)
    ~(store_alloc: ('k, 'v,'r) frame -> ('r,'t) m)
  =
  true


let mk_store_ops ~store_free ~store_read ~store_alloc =
  assert(wf_store_ops ~store_free ~store_read ~store_alloc);
  `Store_ops(store_free,store_read,store_alloc)


let dest_store_ops (`Store_ops(store_free,store_read,store_alloc)) =
  assert(wf_store_ops ~store_free ~store_read ~store_alloc);
  fun k -> k ~store_free ~store_read ~store_alloc


let _ = wf_store_ops

let _ = dest_store_ops


