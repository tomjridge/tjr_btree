(** A collection of the main interfaces *)


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
include Map_ops_type



module Marshalling_ops_type = struct

  (** A type for recording the marshalling functions *)

  type ('dnode,'blk) marshalling_ops = {
    dnode_to_blk: 'dnode -> 'blk;
    blk_to_dnode: 'blk -> 'dnode;
    marshal_blk_size: int
  }
end
include Marshalling_ops_type



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

  type ('k,'v,'t) map_ops_etc = {
    find: k:'k -> ('v option, 't)m;
    insert: k:'k -> v:'v -> (unit,'t) m;
    delete: k:'k -> (unit,'t)m;
    insert_many: k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m;
    insert_all: kvs:('k * 'v) list -> (unit, 't) m;    
  }

end
