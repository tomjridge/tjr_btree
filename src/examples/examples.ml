(** Various examples *)
(* open Tjr_profile.Util.Profiler *)
open Tjr_btree
open Btree_intf

(** The steps to construct an example are:

- fix the monad type (eg store passing)
- construct block ops {!Tjr_fs_shared.Block_ops}, which converts
  strings/bytes to/from blks
- construct block device ops {!Tjr_fs_shared.Blk_dev_ops_type}, which
  reads and writes to blks
- implement {!Bin_prot_marshalling.node_leaf_conversions} to convert
  from list-based leaf/node impls to the efficient leaf/node impls
- implement marshalling procedures via {!Bin_prot_marshalling}
- calculate constants based on blk_sz and key and value types, and
  marshalling strategy
- implement blk_allocator_ops, to allow allocation of blks via id
- for every desired combination of (key/value types, marshalling,
  blk_dev, blk_allocator), use {!Tjr_btree.disk_to_store} to
  construct a corresponding store
- then use {!Tjr_btree.store_to_map} to convert store to a map
  (using a root pointer to convert the pre_map_ops to a map_ops)

FIXME include this documentation in main tjr_btree lib, perhaps as a
simple int->int example

{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vQXKtsYnp_Z4gUHTpYZOeLrGGIIQxPQrSSgdnoUylAW269ckYBMaUXz9MlDk8aHd1evYCSJNFGpqRFb/pub?w=960&amp;h=720">
%}

*)


open Example_disk_ops


(** {2 Abstract version} *)

module type S = sig
  type k  (* we assume k_cmp = Pervasives.compare *)
  type v
  type r = blk_id
  type t = fstore_passing

  val monad_ops: t monad_ops

  val k_size: int
  val v_size: int          
end

(* FIXME the result sig of following could well be from isa_btree *)

module Internal(S:S) = struct
  open S
      
  let k_cmp : k -> k -> int = Pervasives.compare

  let constants = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size 

  let print_constants () = 
    let cs = constants in
    if !Global.debug_global then 
      Printf.printf "Calculated constants: lmin,%d lmax,%d nmin,%d nmax,%d\n%!" 
        cs.min_leaf_size cs.max_leaf_size cs.min_node_keys cs.max_node_keys

  module S' = struct
    include S
    let k_cmp = k_cmp
    let cs = constants
  end
  module Tjr_btree' = Tjr_btree.Make(S')
  (* let node_ops,leaf_ops = Tjr_btree'.(node_ops,leaf_ops) *)
  open Tjr_btree'

  let node_leaf_list_conversions = Node_leaf_list_conversions.{
      node_to_krs=node_ops.node_to_krs;
      krs_to_node=node_ops.krs_to_node;
      leaf_to_kvs=leaf_ops.leaf_to_kvs;
      kvs_to_leaf=leaf_ops.kvs_to_leaf
    }

  let make_map_ops_etc ~blk_dev_ops ~reader_writers ~root_ops = 
    let disk_ops = Example_disk_ops.example_disk_ops ~blk_dev_ops
        ~reader_writers ~node_leaf_list_conversions
    in
    disk_to_map ~disk_ops ~root_ops

  module Export = struct
    let constants = constants
    let print_constants = print_constants
    type nonrec leaf = leaf
    type nonrec node = node
    type nonrec leaf_stream = leaf_stream
    let node_leaf_list_conversions = node_leaf_list_conversions
    let store_to_pre_btree = store_to_pre_btree
    let make_map_ops_etc = make_map_ops_etc
  end
end



(** {2 Instantiations} *)

module type T = 
sig
  type k
  type v
  type r
  val constants : constants
  val print_constants : unit -> unit
  type leaf
  type node
  type leaf_stream
  val node_leaf_list_conversions :
    (k, v, blk_id, node, leaf)
      Node_leaf_list_conversions.node_leaf_list_conversions
  val store_to_pre_btree :
    store_ops:(blk_id,(node,leaf)dnode,fstore_passing) store_ops -> 
    (k, v, r, fstore_passing, leaf, node, leaf_stream) pre_btree_ops
  val make_map_ops_etc: 
    blk_dev_ops:(r, blk, fstore state_passing) blk_dev_ops ->
    reader_writers:(k, v) Bin_prot_marshalling.reader_writers ->
    root_ops:(r, fstore state_passing) btree_root_ops ->
    (k, v, r, leaf_stream, fstore state_passing) Map_ops_etc_type.map_ops_etc
end

module Int_int : T with type k=int and type v=int and type r=int 
= struct
  module S = struct
    open Bin_prot_marshalling
    type k = int
    type v = int
    type r = blk_id
    type t = fstore_passing

    let monad_ops = Monad_ops.monad_ops

    let k_size = int_bin_prot_info.max_size
    let v_size = int_bin_prot_info.max_size
  end
  include S
  module Internal = Internal(S)
  include Internal.Export
  let _ = print_constants()
end


module Ss_ss : T with type k=ss and type v=ss and type r=int 
= struct
  module S = struct
    open Bin_prot_marshalling
    type k = ss
    type v = ss
    type r = blk_id
    type t = fstore_passing

    let monad_ops = Monad_ops.monad_ops

    let k_size = ss_bin_prot_info.max_size
    let v_size = ss_bin_prot_info.max_size
  end
  include S
  module Internal = Internal(S)
  include Internal.Export
  let _ = print_constants()
end


module Ss_int : T with type k=ss and type v=int and type r=int 
= struct
  module S = struct
    open Bin_prot_marshalling
    type k = ss
    type v = int
    type r = blk_id
    type t = fstore_passing

    let monad_ops = Monad_ops.monad_ops

    let k_size = ss_bin_prot_info.max_size
    let v_size = int_bin_prot_info.max_size
  end
  include S
  module Internal = Internal(S)
  include Internal.Export
  let _ = print_constants()
end

