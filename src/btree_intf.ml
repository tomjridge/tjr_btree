(** A collection of the main interfaces. This should be read first. *)


(** A type for recording the marshalling functions *)
type ('dnode,'blk) dnode_mshlr = {
  dnode_to_blk: 'dnode -> 'blk;
  blk_to_dnode: 'blk -> 'dnode;
  blk_sz: blk_sz
}


(** It is common for marshalling to go via a representation using
    lists. This allows us to assume little about node and leaf
    implementations. These fields have the same names and types as
    the corresponding fields in node_ops and leaf_ops. *)
type ('k,'v,'r,'node,'leaf) node_cnvs = {
  node_to_krs: 'node -> 'k list * 'r list;
  krs_to_node: ('k list * 'r list) -> 'node;
  leaf_to_kvs: 'leaf -> ('k * 'v) list;
  kvs_to_leaf: ('k * 'v) list -> 'leaf;
}


(** The B-tree code exports a [pre_map_ops] version of a map, with
   explicit passing of the reference to the root of the B-tree. In
   order to implement the [map_ops] interface, we need to store the
   "current" reference to the B-tree root in the global state
   somehow. A value of type [('r,'t) btree_root_ops] reveals how to read and
   write this reference in the global state.  
*)

(** Btree root; just an int/blk_id *)
type ('r,'t) btree_root_ops = ('r,'t)with_state


(* module Map_ops_etc_type = struct *)
(** As map ops, but with leaf_stream operations *)
module Map_ops_with_ls = struct

  type ('k,'v,'r,'leaf_stream,'t) map_ops_with_ls = {
    find: k:'k -> ('v option, 't)m;
    insert: k:'k -> v:'v -> (unit,'t) m;
    delete: k:'k -> (unit,'t)m;
    insert_many: k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m;
    insert_all: kvs:('k * 'v) list -> (unit, 't) m;    
    leaf_stream_ops: ('k,'v,'r,'leaf_stream,'t)Isa_btree_intf.leaf_stream_ops;
  }

end
type ('k,'v,'r,'leaf_stream,'t) map_ops_with_ls = 
  ('k,'v,'r,'leaf_stream,'t) Map_ops_with_ls.map_ops_with_ls

module Disk_ops_type = struct
  
  (** A collection of block-based interfaces *)
  type ('r,'t,'dnode,'blk) disk_ops = {
    dnode_mshlr:('dnode,'blk)dnode_mshlr;
    blk_dev_ops:('r,'blk,'t)blk_dev_ops;
    blk_alloc:('r,'t)blk_allocator_ops
  }

end
include Disk_ops_type


(** {2 Class interfaces} *)


(* NOTE previously in make_3 *)

type finished = {finished:bool}

class type ['k, 'v, 't ] ls = object
  method ls_step: unit -> (finished,'t)m
  method ls_kvs: unit -> ('k*'v)list
end

(** NOTE uncached here means "no write back cache"; a read cache is
   not observable except for performance and memory usage *)    
class type ['k, 'v, 't ] uncached_btree = 
  object
    method find        : 'k -> ('v option,'t)m
    method insert      : 'k -> 'v -> (unit,'t)m
    method delete      : 'k -> (unit,'t)m
    (* method insert_many : ('k*'v) -> ('k*'v)list -> (('k*'v)list,'t)m *)
    (* method insert_all  : ('k*'v)list -> (unit,'t)m *)
    method ls_create   : unit -> (('k,'v,'t)ls,'t)m
    (* method empty_leaf_as_blk: unit -> ba_buf *)
  end 

module type Bin_mshlr = sig
  type t[@@deriving bin_io]
  val max_sz: int
end

type 'a bin_mshlr = (module Bin_mshlr with type t='a)



(** {2 Util} *)


(* $(FIXME("Move this to shared")) *)

class ['a] set_once = object
  val mutable x = ((Obj.magic ()):'a)
  val mutable is_set = false
  method get =
    assert(is_set);
    x
  method set y = 
    assert(not is_set);
    x<-y
  method is_set = is_set
end



(** {2 For [Make_5] } *)

(* replaced with shared/bp_mshlr

module type TYPE_WITH_MSHLR = sig
  type t[@@deriving bin_io]
end

type 'a type_with_mshlr = (module TYPE_WITH_MSHLR with type t='a) 
*)

type ('a,'t) with_btree_root = ('a,'t)with_state
