(** Store operations: alloc, free and read. The store is a level above
   the raw disk block device, on which we build the B-tree. *)

open Frame
open Tjr_monad.Types


type ('k,'v,'r,'t) store_ops = {
  store_free: 'r list -> (unit,'t) m;
  store_read: 'r -> (('k, 'v,'r) frame,'t) m;
  store_alloc: ('k, 'v,'r) frame -> ('r,'t) m;
}

(* store ------------------------------------------------------------ *)

let mk_store_ops ~store_free ~store_read ~store_alloc =
  {store_free; store_read; store_alloc }


let dest_store_ops r =
  let store_free,store_read,store_alloc = r.store_free,r.store_read,r.store_alloc in
  fun k -> k ~store_free ~store_read ~store_alloc

let _ = dest_store_ops

