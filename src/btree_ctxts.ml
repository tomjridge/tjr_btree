(** This is an attempt to tame the proliferation of type variables, by
   introducing "contexts" which fix types and some values. We use C_ as a prefix *)
open Btree_intf

module type C_kvrt = sig
  type k
  type v
  type r
  type t
  val k_cmp: k -> k -> int
  val monad_ops: t monad_ops
  val ( >>= ) : ('a, t) m -> ('a -> ('b, t) m) -> ('b, t) m
  val return : 'a -> ('a, 't) m
  val cs: Isa_btree.Constants.constants
end

module type C_leaf_node_leaf_stream = sig
  include C_kvrt
  type leaf
  type node
  type leaf_stream
  type nonrec dnode = (node,leaf)dnode
  type nonrec leaf_ops = (k,v,leaf) Isa_btree_intf.leaf_ops
  type nonrec node_ops = (k,v,leaf) Isa_btree_intf.node_ops
  type nonrec nlc = (k,v,r,node,leaf) nlc
  type nonrec store_ops = (r,(node,leaf)Isa_btree.dnode,t)Isa_btree.store_ops
  type nonrec pre_btree_ops = (k,v,r,t,leaf,node,leaf_stream) Isa_btree.pre_btree_ops
  type nonrec map_ops_with_ls = (k,v,r,leaf_stream,t)Map_ops_with_ls.map_ops_with_ls
  type nonrec root_ops = (r,t)btree_root_ops

  val leaf_ops: leaf_ops
  val node_ops: node_ops
  val node_leaf_list_conversions: nlc
  val store_to_pre_btree: store_ops:store_ops -> pre_btree_ops
  (* val pre_btree_to_map: pre_btree_ops:pre_btree_ops -> root_ops:root_ops -> map_ops_with_ls *)
end

(* following should be in bt_examples *)

module type C_blk = sig
  type blk_id
  type blk
  type t
  type nonrec blk_ops = blk blk_ops
  type nonrec blk_dev_ops = (blk_id,blk,t)blk_dev_ops (* assume blk_sz = blk_ops.blk_sz *)
end

module type C_blk_store = sig
  include C_blk
  type nonrec blk_allocator_ops = (blk_id,t)blk_allocator_ops
  val blk_allocator_ops: blk_allocator_ops
end

module type C_marshal = sig
  include C_blk
  type k
  type v
  type r = blk_id [@@deriving bin_io]
  type node
  type leaf
  type nonrec dnode = (node,leaf)dnode  (* typically provided by C_leaf_node_leaf_stream *)
  val node_leaf_list_conversions : (k,v,r,node,leaf) nlc
  type nonrec marshalling_ops = (dnode,blk) marshalling_ops
  val marshalling_ops : marshalling_ops
end

(* putting it all together, C_kvrt + C_blk + C_blk_store + C_marshal
   gives C_leaf_node_leaf_stream + store_ops + pre_btree_ops; then
   with root_ops we get to map ops

   the "+" operation on signatures means "merge these, and identify
   similarly-named types" 
*)

(* Given blk_sz, k_sz and v_sz, we can compute constants and bp marshalling *)
module type C_kvr_sizes = sig
  val blk_sz: int
  val k_sz: int
  val v_sz: int
  val cs: Isa_btree.Constants.constants
  (* val reader_writers: (k,v) Bin_prot_marshalling.reader_writers *)
end
