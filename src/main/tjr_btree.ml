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
  type leaf
  type node
  type leaf_stream
  
  val leaf_ops : (k, v, leaf) Isa_btree_intf.leaf_ops
  val node_ops : (k, r, node) Isa_btree_intf.node_ops
  
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
(k, v, r, leaf_stream, t) Map_ops_etc_type.map_ops_etc

  (** Convenience; a composition of the previous. Note that this is not cached. For caching, construct in stages. *)
  val disk_to_map: 
    disk_ops:'blk disk_ops ->
    root_ops:(r, t) btree_root_ops ->
(k, v, r, leaf_stream, t) Map_ops_etc_type.map_ops_etc


  (** Convenience *)
  val empty_leaf_as_blk: disk_ops:'blk disk_ops -> 'blk

end = struct
  open S
  include Isa_btree.Make(S)

  type nonrec 'blk disk_ops = (r,t,(node,leaf)dnode,'blk) disk_ops

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

  (** Convenience *)
  let empty_leaf_as_blk ~disk_ops = 
    disk_ops.marshalling_ops.dnode_to_blk (Disk_leaf (leaf_ops.kvs_to_leaf []))

(*
  let disk_to_leaf_stream ~disk_ops = 
    let store_ops = disk_to_store ~disk_ops in
    let pre_btree_ops = make_btree_ops ~store_ops in
    pre_btree_ops.leaf_stream_ops

  let _ = disk_to_leaf_stream
*)

  type nonrec store_ops = (r, (node, leaf) dnode, t) store_ops

  type nonrec pre_btree_ops = (k, v, r, t, leaf, node, leaf_stream) pre_btree_ops
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
