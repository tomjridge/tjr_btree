(** Main functionality: disk to map *)

open Btree_intf
open Disk_to_store

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

module type T = sig
  type k
  type v
  type r
  type t
  type leaf
  type node
  type leaf_stream
  
  val leaf_ops : (k, v, leaf) Isa_btree_intf.leaf_ops
  val node_ops : (k, r, node) Isa_btree_intf.node_ops
  val node_cnvs : (k, v, r, node, leaf) node_cnvs

  type nonrec 'blk disk_ops = (r,t,(node,leaf)dnode,'blk) disk_ops
  type nonrec store_ops = (r, (node, leaf) dnode, t) store_ops
  type nonrec pre_btree_ops = (k, v, r, t, leaf, node, leaf_stream) pre_btree_ops
  type nonrec map_ops_with_ls = (k, v, r, leaf_stream, t) map_ops_with_ls

  val disk_to_store: disk_ops:'blk disk_ops -> store_ops

  val store_to_pre_btree : store_ops:store_ops -> pre_btree_ops

  val pre_btree_to_map: 
    pre_btree_ops:pre_btree_ops ->
    root_ops:(r, t) btree_root_ops ->
    map_ops_with_ls

  (** Convenience; a composition of the previous. Note that this is not cached. For caching, construct in stages. *)
  val disk_to_map: 
    disk_ops:'blk disk_ops ->
    root_ops:(r, t) btree_root_ops ->
    map_ops_with_ls
end

(** The basic Make functor, with most things parameterizable. See also the more refined {!Tjr_btree_examples.Make_example} functor. *)
module Make(S:S) : T with type k=S.k and type v=S.v and type r=S.r and type t=S.t = struct
  include S
  include Isa_btree.Make(S)

  type nonrec 'blk disk_ops = (r,t,(node,leaf)dnode,'blk) disk_ops

  let node_cnvs = {
    node_to_krs=node_ops.node_to_krs;
    krs_to_node=node_ops.krs_to_node;
    leaf_to_kvs=leaf_ops.leaf_to_kvs;
    kvs_to_leaf=leaf_ops.kvs_to_leaf
  }

  let disk_to_store ~disk_ops = 
    let { dnode_mshlr; blk_dev_ops; blk_alloc } = disk_ops in
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

  type nonrec map_ops_with_ls = 
    (k, v, r, leaf_stream, t) map_ops_with_ls
end



(*
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
*)
