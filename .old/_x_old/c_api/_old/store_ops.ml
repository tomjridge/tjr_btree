(** Store operations: alloc, free and read. The store is a level above
   the raw disk block device, on which we build the B-tree.

NOTE this module safe to open

FIXME we may want to have an explicit "device passing" ops type to
   complement the blk_dev type

*)

open Frame
open Tjr_monad.Types

type ('k,'v,'r,'t) store_ops = {
  store_free: 'r list -> (unit,'t) m;
  store_read: 'r -> (('k, 'v,'r) frame,'t) m;
  store_alloc: ('k, 'v,'r) frame -> ('r,'t) m;
}

