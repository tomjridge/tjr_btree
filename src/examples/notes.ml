(** Some notes on various construction paths *)


open Btree_intf

open Map_ops_etc_type

(** The block layer... *)


(** The target type. Together with root ops we can get to
   map_ops_etc. The difficulty is dnode, which is ('node,'leaf)dnode
   for some node,leaf implementations. *)
type ('r,'dnode,'t) store_ops = ('r,'dnode,'t)Isa_btree_intf.store_ops


(** The block layer types... *)

open Node_leaf_list_conversions
open Bin_prot_marshalling

module type GENERIC_FUNCTIONS = sig
  val disk_to_store:
    't monad_ops -> 
    ('r,'t,'dnode,'blk)disk_ops -> 
    ('r,'dnode,'t)store_ops

  val pre_btree_to_map: 
    't monad_ops -> 
    ('k,'v,'r,'t,'leaf,'node,'leaf_stream)pre_btree_ops ->
    ('r,'t)btree_root_ops -> 
    ('k,'v,'r,'leaf_stream,'t)map_ops_etc
end

module type B = functor
  (S0: sig
     type blk  (* can be free for bin_prot via block_ops *)
     val block_ops : blk block_ops
     type r = int  (* for marshalling *)

     type t
     val monad_ops: t monad_ops 

     (* when dealing with the block layer, it is normal to fix this *)
     val blk_dev_ops: (r,blk,t) blk_dev_ops end) 

  -> 
    (sig
      open S0

      val blk_allocator_ops : (r,t)blk_allocator_ops

      (* For marshalling, we need k,v; we try to avoid fixing node and
         leaf types *)
      module type With_kv = functor
        (S1: sig
           type k
           type v
           val reader_writers: (k,v)reader_writers
         end) 
        -> 
          (sig
            open S1
                (*
            val make_binprot_marshalling: 
              (k,v,r,'node,'leaf)node_leaf_list_conversions ->
              (('node,'leaf)dnode,blk) marshalling_ops
*)
            (* uses make_binprot_marshalling *)
            val make_disk_ops: 
               (* Constants.constants -> not needed? *)
              (k,v,r,'node,'leaf)node_leaf_list_conversions ->
              (r,t,('node,'leaf)dnode,blk)disk_ops
          end)
    end)


(*

(* The B-tree root ops *)
module type C = functor
  (S0: sig
     type blk_id (* can be free for this construction, but needs to be
                    int for marshalling *)
     type t = fstore_passing
     val fstore_ref : blk_id Tjr_store.Refs.r  (* an int to hold the b-tree root *) 
   end)
  -> 
    (sig
      open S0
      val btree_root_ops: (blk_id,fstore_passing) btree_root_ops
    end)
*)


(** Now we should think about the blk_dev_util, initialization etc *)
module type BLK_DEV_ON_FD_UTIL = sig

  open Blk_layer
  
  val btree_from_file:     
    block_ops:'a block_ops ->
    empty_disk_leaf_as_blk:(unit -> 'a) -> btree_from_file

end




(* From store_ops to pre_btree_ops (Isa_btree) and map_ops_etc
   (Tjr_btree); a summary of store-ops onwards *)

module type D = functor
  (S: sig
     type k
     val k_cmp: k -> k -> int

     type t
     val monad_ops:t monad_ops

     val cs: Constants.constants

     type v
     type r
   end) -> 
  (sig
    open S
    type node
    type leaf

    (* FIXME this can be added to pre_btree_ops *)
    val node_leaf_list_conversions: (k,v,r,node,leaf) node_leaf_list_conversions

    type leaf_stream
    val pre_btree_ops: (k,v,r,t,leaf,node,leaf_stream) pre_btree_ops
  end)


(* So the mediation for block/non-block is between node_leaf_list_conversions and cs *)



(*
module type A = functor 
  (A : sig
     type r
     type t
     open Node_leaf_list_conversions
     val disk_ops: 
       ('k,'v,r,'node,'leaf) node_leaf_list_conversions -> 
       (r,t,('node,'leaf)dnode,'blk)disk_ops
     val root_ops: (r,t)btree_root_ops
   end) 
  -> 
    (sig
      open A
      type leaf_stream
      val map_ops_etc: (k,v,r,leaf_stream,t)Map_ops_etc_type.map_ops_etc
    end
                           

module type K = sig
  type k
  val k_cmp: k -> k -> int
end



(** So, let's, fix these *)

module type A = functor
  (S: sig
     type node
     type leaf
     type nonrec dnode = (node,leaf)dnode
   end) -> 
  (sig
    end)


*)
