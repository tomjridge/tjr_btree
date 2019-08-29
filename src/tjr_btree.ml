(** Export the main functionality of this library. *)

(** {2 Main interfaces} *)

module Btree_intf = Btree_intf
open Btree_intf

(** {2 Disk_to_store} *)

include Disk_to_store


(** {2 Store to pre-btree} *)

(** This is provided by the {!Isa_btree} package. *)


(** {2 Pre-btree to map} *)

include Pre_btree_to_map


(** {2 Main functionality: disk to map} *)

(** The main functionality of this package: take some parameters
   describing the disk, and produce a map interface. This is a
   composition of Disk_to_store, store_to_pre_btree and
   pre_btree_to_map. *)

(** This interface should be equal to {!Isa_btree_intf.S}. Duplicated
   here so that ocamldoc can exhibit the definition. *)
module type S = (* Isa_btree_intf.S *) sig
    type k
    type v
    type r
    type t
    val k_cmp: k -> k -> int
    val monad_ops: t monad_ops
    val cs: Constants.constants
  end

module Make(S:S) : sig
  open S
  open Node_leaf_list_conversions

  type leaf
  type node
  type leaf_stream
  
  val leaf_ops : (k, v, leaf) Isa_btree_intf.leaf_ops
  val node_ops : (k, r, node) Isa_btree_intf.node_ops

  val node_leaf_list_conversions : (k, v, r, node, leaf) node_leaf_list_conversions

  type nonrec 'blk disk_ops = (r,t,(node,leaf)dnode,'blk) disk_ops

  type nonrec store_ops = (r, (node, leaf) dnode, t) store_ops

  type nonrec pre_btree_ops = (k, v, r, t, leaf, node, leaf_stream) pre_btree_ops

  val disk_to_store: 
    disk_ops:'blk disk_ops ->
    store_ops

  val store_to_pre_btree :
    store_ops:store_ops ->
    pre_btree_ops

  val pre_btree_to_map: 
pre_btree_ops:pre_btree_ops ->
root_ops:(r, t) btree_root_ops ->
(k, v, r, leaf_stream, t) Map_ops_with_ls.map_ops_with_ls

  (** Convenience; a composition of the previous. Note that this is not cached. For caching, construct in stages. *)
  val disk_to_map: 
    disk_ops:'blk disk_ops ->
    root_ops:(r, t) btree_root_ops ->
(k, v, r, leaf_stream, t) Map_ops_with_ls.map_ops_with_ls


  (** Convenience; dnode_to_blk comes from disk_ops; NOTE we try to
     avoid restrictions on the blk type *)
  (* val empty_leaf_as_blk: leaf_to_blk:(leaf -> 'blk) -> 'blk *)

end = struct
  open S
  include Isa_btree.Make(S)

  type nonrec 'blk disk_ops = (r,t,(node,leaf)dnode,'blk) disk_ops

  let node_leaf_list_conversions = Node_leaf_list_conversions.{
      node_to_krs=node_ops.node_to_krs;
      krs_to_node=node_ops.krs_to_node;
      leaf_to_kvs=leaf_ops.leaf_to_kvs;
      kvs_to_leaf=leaf_ops.kvs_to_leaf
    }

  let disk_to_store ~disk_ops = 
    let { marshalling_ops; blk_dev_ops; blk_allocator_ops } = disk_ops in
    let store_ops = disk_to_store ~monad_ops ~disk_ops in
    store_ops

  let store_to_pre_btree ~store_ops = make_btree_ops ~store_ops

  let pre_btree_to_map ~(pre_btree_ops:(k, v, r, t, leaf, node, leaf_stream) pre_btree_ops) ~root_ops = 
    Pre_btree_to_map.pre_btree_to_map ~monad_ops ~pre_btree_ops ~root_ops

  let _ = pre_btree_to_map

  let disk_to_map ~disk_ops = 
    let store_ops = disk_to_store ~disk_ops in
    let pre_btree_ops = make_btree_ops ~store_ops in
    fun ~root_ops -> 
    Pre_btree_to_map.pre_btree_to_map ~monad_ops ~pre_btree_ops ~root_ops
    
  let _ = disk_to_map

  (** Convenience; apply dnode_to_blk to an empty leaf *)
  let empty_leaf_as_blk ~(dnode_to_blk:(node,leaf)dnode -> 'blk) : 'blk = 
    dnode_to_blk (Disk_leaf (leaf_ops.kvs_to_leaf []))

  let _ = empty_leaf_as_blk

  type nonrec store_ops = (r, (node, leaf) dnode, t) store_ops

  type nonrec pre_btree_ops = 
    (k, v, r, t, leaf, node, leaf_stream) pre_btree_ops
end



(** {2 Internal interface} *)

module Internal = struct
  type ('a,'b,'c) tmp = {
    store_ops:'a;
    pre_btree_ops:'b;
    map_ops:'c;
  }

  (** Note that this takes comparators for the node and leaf implementations *)
  let internal_disk_to_x ~monad_ops ~cs ~k_cmp ~kopt_cmp ~disk_ops ~root_ops =
    (* let { marshalling_ops; blk_dev_ops; blk_allocator_ops } = disk_ops in *)
    let store_ops = disk_to_store ~monad_ops ~disk_ops in
    let pre_btree_ops = Isa_btree.make_with_comparators ~monad_ops ~cs ~k_cmp ~kopt_cmp ~store_ops in
    let map_ops = Pre_btree_to_map.pre_btree_to_map ~monad_ops ~pre_btree_ops ~root_ops in
    { store_ops; pre_btree_ops; map_ops }
end

let internal_disk_to_x = Internal.internal_disk_to_x
