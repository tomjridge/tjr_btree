(** Like make_1, but using the classes from {!Btree_intf_v2}, and bin_prot marshalling *)


[@@@warning "-33"]

open Btree_intf
open Make_1


module type S = (* Isa_btree_intf.S *) sig
  type k
  type v
  type r
  type t
  val k_cmp: k -> k -> int
  val monad_ops: t monad_ops
  val cs: Constants.constants

  val r_cmp: r -> r -> int (* for wbc *)
end


module Make(S:S) = struct

  module Pvt = struct
    module M1 = Make_1.Make(S)
    open M1

    module W_ = Write_back_cache.Make_write_back_cache
        (struct type t = r let compare = S.r_cmp end)
        (struct type t = (node,leaf)Isa_btree.dnode end)

    module S2 = struct
      include S
      type blk = ba_buf
      type nonrec leaf = leaf
      type nonrec node = node
      type ls = leaf_stream
      type wb = W_.Internal.Lru.t
    end

    (** virtual classes *)
    module V = Btree_intf_v2.Make(S2)
    

  end
  open Pvt
  open Pvt.M1
  open V.Abbrevs


  class c1 = object
    inherit V.c1
    method monad_ops = S.monad_ops
    method k_cmp = S.k_cmp
    method cs = S.cs
  end

  class virtual c2 = object (self)
    inherit c1 
    inherit V.c2
    method node_cnvs = node_cnvs
    method pre_btree_ops = store_to_pre_btree ~store_ops:self#store_ops
  end

  class virtual c3 = object (self)
    inherit c2
    inherit V.c3
    method map_ops_with_ls =
      pre_btree_to_map ~pre_btree_ops:(self#pre_btree_ops)
        ~root_ops:(self#with_btree_root)
  end
  
  type blk = ba_buf


  class virtual c4 = object (self)
    inherit V.c4
    method uncached_store_ops = 
      disk_to_store ~disk_ops:{
        dnode_mshlr=self#dnode_mshlr;
        blk_dev_ops=self#blk_dev_ops;
        blk_alloc=self#blk_alloc
      }
    method empty_leaf_as_blk = empty_leaf_as_blk ~dnode_to_blk:(self#dnode_mshlr.dnode_to_blk)
  end
    
  class virtual c5 = 
    object (self)
      inherit V.c5

      val pvt_x : (dnode,blk)dnode_mshlr set_once = new set_once
      method dnode_mshlr = pvt_x#get
      
      method node_cnvs = node_cnvs

      initializer
        let (k,v,r) = self#kvr_mshlrs in
        let module K = (val k) in
        let module V = (val v) in
        let module R = (val r) in
        let open (struct
          module Blk_id = Blk_id_as_int
          module X = struct
            type blk_id = R.t[@@deriving bin_io]
            type k = K.t[@@deriving bin_io]
            type v = V.t[@@deriving bin_io]
            type blk = ba_buf
            let blk_sz = self#blk_sz
            type nonrec node = node
            type nonrec leaf = leaf
            let node_cnvs = node_cnvs
          end

          module Y = Bin_prot_marshalling.Make(X)
        end)
        in
        pvt_x#set Y.dnode_mshlr        
      
    end



  class virtual c7 = object (self)
    inherit c4
    inherit V.c7

    val pvt_y : _ set_once = new set_once

    method write_back_cache_ops = (pvt_y#get).ops
    method empty_write_back_cache = (pvt_y#get).initial_state

    val pvt_z : _ set_once = new set_once
    method store_ops_with_cache = pvt_z#get


    initializer
      let open (struct
        (* type write_back_cache = W_.Internal.Lru.t *)
        (* let make_write_back_cache = W_.make_write_back_cache *)
        let _ : unit = pvt_y#set (W_.make_write_back_cache ~cap:5200 ~delta:10)

        let evict writes = 
          (* these writes are dnodes; we need to marshall them first *)
          writes |> List.map (fun (blk_id,dn) -> 
              (blk_id,self#dnode_mshlr.dnode_to_blk dn))
          |> self#blk_dev_ops.write_many        

        let store_ops_with_cache = Store_write_back_cache.add_write_back_cache_to_store
            ~monad_ops:S.monad_ops
            ~uncached_store_ops:self#uncached_store_ops
            ~alloc:self#blk_alloc.blk_alloc
            ~evict
            ~write_back_cache_ops:self#write_back_cache_ops
            ~with_write_back_cache:self#with_write_back_cache
      end)
      in
      pvt_z#set store_ops_with_cache
                
  end

  class virtual c8 = object
    inherit c3
    inherit! c5
    inherit c7
  end
(** {[
      method virtual blk_alloc : (S.r, S.t) Tjr_fs_shared.blk_allocator_ops
      method virtual blk_dev_ops : Pvt.V.Abbrevs.blk_dev_ops
      method virtual blk_sz : Tjr_fs_shared.blk_sz
      method cs : Isa_btree.constants
      method dnode_mshlr :
        (Pvt.V.Abbrevs.dnode, Pvt.S2.blk) Tjr_btree__.Btree_intf.dnode_mshlr
      method empty_leaf_as_blk : Pvt.S2.blk
      method empty_write_back_cache : Pvt.S2.wb
      method k_cmp : S.k -> S.k -> int
      method virtual kvr_mshlrs :
        S.k Tjr_btree__Btree_intf_v2.type_with_mshlr *
        S.v Tjr_btree__Btree_intf_v2.type_with_mshlr *
        S.r Tjr_btree__Btree_intf_v2.type_with_mshlr
      method map_ops_with_ls : Pvt.V.Abbrevs.map_ops_with_ls
      method monad_ops : S.t Tjr_monad.monad_ops
      method node_cnvs : Pvt.V.Abbrevs.node_cnvs
      method pre_btree_ops : Pvt.V.Abbrevs.pre_btree_ops
      method virtual store_ops : Pvt.V.Abbrevs.store_ops
      method store_ops_with_cache : Pvt.V.Abbrevs.store_ops
      method uncached_store_ops : Pvt.V.Abbrevs.store_ops
      method virtual with_btree_root :
        (S.r, S.t) Tjr_btree__Btree_intf_v2.with_btree_root
      method virtual with_write_back_cache :
        (Pvt.S2.wb, S.t) Tjr_monad.with_state
      method write_back_cache_ops :
        (S.r, Pvt.V.Abbrevs.dnode, S.r * Pvt.V.Abbrevs.dnode, Pvt.S2.wb)
        Tjr_btree__Btree_intf_v2.wbc_ops
]} *)



end

