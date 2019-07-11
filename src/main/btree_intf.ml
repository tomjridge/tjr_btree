(** A collection of the main interfaces. This should be read first.

In particular, note the following:

- [marshalling_ops], for marshalling to disk
- [disk_ops], a collection of the block-layer operations
- [btree_root_ops], which details how to update the B-tree root when it changes

Then the main functionality of this library is provided by the {!Tjr_btree.Make} functor, which provides [disk_to_map], to convert [disk_ops] to a map (and other operations).

 *)


module Blk_allocator_ops_type = struct
  (** A type for managing the free space on the disk. *)
  (* open Tjr_monad.Types *)

  (** NOTE we assume alloc never fails, or that error is handled
      elsewhere in the monad *)
  type ('blk_id,'t) blk_allocator_ops = {
    alloc: unit -> ('blk_id,'t) m; 
    free: 'blk_id -> (unit,'t) m;
  }
end
include Blk_allocator_ops_type



module Map_ops_type = struct

(** Map operations *)

  (* open Tjr_monad.Types *)

  type ('k,'v,'t) map_ops = {
    find: k:'k -> ('v option, 't)m;
    insert: k:'k -> v:'v -> (unit,'t) m;
    delete: k:'k -> (unit,'t)m;
  }

(*
type ('k,'v,'r,'leaf,'ls_impl,'t) extra_map_ops = {
  insert_many:
  insert_all: kvs:('k*'v)list -> (unit, 't)m;
  leaf_stream_ops: ('k,'v,'r,'ls_impl,'t) leaf_stream_ops;
}
*)
  type ('a,'b,'c) extra_map_ops = {
    insert_many: 'a;
    insert_all: 'b;
    leaf_stream_ops: 'c;
  }
end
(* include Map_ops_type *)



module Marshalling_ops_type = struct

  (** A type for recording the marshalling functions *)
  type ('dnode,'blk) marshalling_ops = {
    dnode_to_blk: 'dnode -> 'blk;
    blk_to_dnode: 'blk -> 'dnode;
    marshal_blk_size: int
  }
end
include Marshalling_ops_type



module Node_leaf_list_conversions = struct
  (** It is common for marshalling to go via a representation using
     lists. This allows us to assume little about node and leaf
     implementations. These fields have the same names and types as
     the corresponding fields in node_ops and leaf_ops. *)
  type ('k,'v,'r,'node,'leaf) node_leaf_list_conversions = {
    node_to_krs: 'node -> 'k list * 'r list;
    krs_to_node: ('k list * 'r list) -> 'node;
    leaf_to_kvs: 'leaf -> ('k * 'v) list;
    kvs_to_leaf: ('k * 'v) list -> 'leaf;
  }
end
(* don't include, since fields clash with those from node_ops/leaf_ops *)


(** The B-tree code exports a [pre_map_ops] version of a map, with
   explicit passing of the reference to the root of the B-tree. In
   order to implement the [map_ops] interface, we need to store the
   "current" reference to the B-tree root in the global state
   somehow. A value of type [('r,'t) btree_root_ops] reveals how to read and
   write this reference in the global state.  
*)
module Root_ops_type = struct

  type ('r,'t) btree_root_ops = {
    root_ops: ('r,'t)with_state
  }

end
include Root_ops_type


module Map_ops_etc_type = struct

  type ('k,'v,'r,'leaf_stream,'t) map_ops_etc = {
    find: k:'k -> ('v option, 't)m;
    insert: k:'k -> v:'v -> (unit,'t) m;
    delete: k:'k -> (unit,'t)m;
    insert_many: k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m;
    insert_all: kvs:('k * 'v) list -> (unit, 't) m;    
    leaf_stream_ops: ('k,'v,'r,'leaf_stream,'t)Isa_btree_intf.leaf_stream_ops;
  }

end


module Disk_ops_type = struct
  
  (** A collection of block-based interfaces *)
  type ('r,'t,'dnode,'blk) disk_ops = {
    marshalling_ops:('dnode,'blk)marshalling_ops;
    blk_dev_ops:('r,'blk,'t)blk_dev_ops;
    blk_allocator_ops:('r,'t)blk_allocator_ops
  }

end
include Disk_ops_type
