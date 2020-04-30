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
end


module Make(S:S) = struct

  module Pvt = struct
    module M1 = Make_1.Make(S)
    open M1

    module S2 = struct
      include S
      type blk = ba_buf
      type nonrec leaf = leaf
      type nonrec node = node
      type ls = leaf_stream
      type wb
    end

    (** virtual classes *)
    module V = Btree_intf_v2.Make(S2)

  end
  open Pvt
  open Pvt.M1


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
    
  open V.Abbrevs

  open Btree_intf_v2

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


end

