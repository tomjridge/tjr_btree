(** Like make_1, but using bin_prot marshalling *)


[@@@warning "-33"]

open Btree_intf
(* open Btree_intf_v2 *)
open Make_1
open Write_back_cache

module type S1 = sig
  type k
  type v
  type r 
  type t
  type ls
  type blk
  type dnode
  type wbc
end

(** Make the btree_stack class type *)
module Btree_stack = struct

  (* type nonrec blk_dev_ops = (r,blk,t)blk_dev_ops *)
  (* type nonrec store_ops = (r, dnode, t) store_ops *)
  (* type nonrec map_ops_with_ls = (k, v, r, ls, t) map_ops_with_ls *)

  (* $(EXECC("""sed -n '/type[ ]btree_stack/,/end/p' >GEN.btree_stack.ml_""")) *)
  (** The following type is what we depend on in subsequent code; in
     this file, we provide something stronger - a virtual class, which
     we can mix in with other classes to provide the final class from
     which we create the object of type btree_stack *)
  type ('k,'v,'r,'t,'ls,'blk,'dnode,'wbc) btree_stack = <
    monad_ops             : 't monad_ops;

    blk_alloc             : ('r, 't) blk_allocator_ops;
    blk_dev_ops           : ('r,'blk,'t) blk_dev_ops;
    blk_sz                : blk_sz;

    empty_leaf_as_blk     : 'blk;

    wbc_o                 : ('r,'dnode,'wbc) wbc_o;
    with_write_back_cache : ('wbc, 't) with_state;
    flush_wbc             : unit -> (unit, 't) m;
   
    store_ops             : ('r,'dnode,'t) store_ops;

    with_btree_root       : ('r, 't) with_btree_root;
    map_ops_with_ls       : ('k,'v,'r,'ls,'t) map_ops_with_ls;
  >

(*

    kvr_mshlrs            :
      k type_with_mshlr *
      v type_with_mshlr *
      r type_with_mshlr;
*)

end


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

  type k = S.k
  type v = S.v
  type r = S.r
  type t = S.t
  type blk = ba_buf

  module M1 = Make_1.Make(S)
  open M1

  type node = M1.node
  type leaf = M1.leaf
  type ls = M1.leaf_stream
  type nonrec dnode = (node,leaf)dnode


  module W_ = Write_back_cache.Make
      (struct type t = r let compare = S.r_cmp end)
      (struct type t = (node,leaf)Isa_btree.dnode end)


  type wbc = W_.wbc

  (* some abbrevs *)
  type nonrec blk_dev_ops = (r,blk,t)blk_dev_ops
  type nonrec node_cnvs = (k,v,r,node,leaf)node_cnvs


  (** WARNING the order or initialization for classes is rather fiddly;
     in particular, virtual slots like blk_dev_ops need to be defined
     in the subclass proper, not in the class initializer *)

  class c1 = object 
    method monad_ops = S.monad_ops
    method k_cmp = S.k_cmp
    method cs = S.cs
  end

  class virtual c2 = object
    inherit c1
    method virtual blk_alloc : (r, t) blk_allocator_ops
    method virtual blk_dev_ops : blk_dev_ops
    method virtual blk_sz : blk_sz
  end

  (* dnode_mshlr *)
  class virtual c3 = object (self)
    inherit c2

    method virtual k_mshlr: k type_with_mshlr
    method virtual v_mshlr: v type_with_mshlr
    method virtual r_mshlr: r type_with_mshlr

    val pvt_d = new set_once
    method dnode_mshlr = pvt_d#get

    method node_cnvs = node_cnvs

    method empty_leaf_as_blk = 
      empty_leaf_as_blk ~dnode_to_blk:(self#dnode_mshlr.dnode_to_blk)

    initializer
      let (k,v,r) = (self#k_mshlr,self#v_mshlr,self#r_mshlr) in
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
      pvt_d#set Y.dnode_mshlr

  end

  (* wbc *)
  class virtual c4 = object (self)
    inherit c3
    method wbc_o = W_.wbc_factory#make_wbc ~cap:5200 ~delta:10
    method wbc_factory = W_.wbc_factory
    method wbc_evict = fun writes -> 
      (* these writes are dnodes; we need to marshall them first *)
      writes |> List.map (fun (blk_id,dn) -> 
          (blk_id,self#dnode_mshlr.dnode_to_blk dn))
      |> self#blk_dev_ops.write_many        

    method virtual with_write_back_cache : (wbc, t) with_state

    (* val pvt_a = new set_once *)
    method flush_wbc = fun () -> 
      let (>>=) = self#monad_ops.bind in
      self#with_write_back_cache.with_state (fun ~state ~set_state -> 
          self#wbc_o#ops.clean state |> fun (evictees,state) -> 
          self#wbc_evict evictees >>= fun () -> 
          set_state state)

  end


  (* store ops, with and without cache *)
  class virtual c5 = object (self)
    inherit c4

    method uncached_store_ops = 
      disk_to_store ~disk_ops:{
        dnode_mshlr=self#dnode_mshlr;
        blk_dev_ops=self#blk_dev_ops;
        blk_alloc=self#blk_alloc
      }

    val pvt_e = new set_once
    method store_ops_with_cache : store_ops = pvt_e#get

    method virtual store_ops: store_ops

    method pre_btree_ops = store_to_pre_btree ~store_ops:(self#store_ops)

    initializer
      pvt_e#set @@ Store_write_back_cache.add_write_back_cache_to_store
        ~monad_ops:S.monad_ops
        ~uncached_store_ops:self#uncached_store_ops
        ~alloc:self#blk_alloc.blk_alloc
        ~evict:self#wbc_evict
        ~write_back_cache_ops:self#wbc_o#ops
        ~with_write_back_cache:self#with_write_back_cache


  end

  class virtual c6 = object (self)
    inherit c5
    method virtual with_btree_root : (r, t) with_btree_root
    method map_ops_with_ls = 
      pre_btree_to_map 
        ~pre_btree_ops:(self#pre_btree_ops)
        ~root_ops:(self#with_btree_root)
  end



  (** The [btree_stack] type should match c6 *)

  module S2 = struct
    type nonrec k     = k    
    type nonrec v     = v    
    type nonrec r     = r    
    type nonrec t     = t    
    type nonrec ls    = ls   
    type nonrec blk   = blk  
    type nonrec dnode = dnode
    type nonrec wbc   = wbc  
  end

  open Btree_stack

  (* check agreement between c6 and btree_stack *)
  let coerce (x:c6) = (x : c6 :> (_,_,_,_,_,_,_,_)btree_stack)

end



(*
  (* check can cast to external type *)
  let _f (x:btree_stack) = (x : btree_stack :> (_,_,_,_,_,_,_,_) btree_stack')

(** This is the result of the Make functor below, with type variables
   rather than fixed types *)
class type virtual ['k,'v,'r,'t,'ls,'blk,'dnode,'wbc] btree_stack' = object
  method cs : constants
  method k_cmp : 'k -> 'k -> int
  method monad_ops : 't monad_ops

  method blk_alloc : ('r, 't) blk_allocator_ops
  method virtual blk_dev_ops : ('r,'blk,'t) blk_dev_ops
  method virtual blk_sz : blk_sz


  method virtual kvr_mshlrs : 'k type_with_mshlr *
                              'v type_with_mshlr *
                              'r type_with_mshlr
  (* method dnode_mshlr : (dnode, blk) dnode_mshlr *)
  method empty_leaf_as_blk : 'blk
  (* method node_cnvs : node_cnvs *)


  method wbc_o : ('r,'dnode,'wbc)wbc_o
  (* method wbc_factory : ('r, 'dnode,'wbc) Write_back_cache.wbc_factory *)
  (* method wbc_evict : ('r * 'dnode) list -> (unit, t) m *)
  method virtual with_write_back_cache : ('wbc, 't) with_state


  method virtual store_ops : ('r,'dnode,'t) store_ops
  method store_ops_with_cache :  ('r,'dnode,'t) store_ops
  method uncached_store_ops : ('r,'dnode,'t) store_ops
  (* method pre_btree_ops : pre_btree_ops *)

  method virtual with_btree_root : ('r, 't) with_btree_root
  method map_ops_with_ls : ('k,'v,'r,'ls,'t) map_ops_with_ls
end
*)


(*
  (* $ (EXECC("""sed -n '/virtual[ ]btree_stack/,/end/p' >btree_stack_export.ml_""")) *)
  class type virtual btree_stack = object
    method monad_ops : t monad_ops

    method virtual blk_alloc : (r, t) blk_allocator_ops
    method virtual blk_dev_ops : blk_dev_ops
    method virtual blk_sz : blk_sz

    method virtual k_mshlr: k type_with_mshlr
    method virtual v_mshlr: v type_with_mshlr
    method virtual r_mshlr: r type_with_mshlr

    method empty_leaf_as_blk : blk

    method wbc_o : (r,dnode,wbc)wbc_o
    method virtual with_write_back_cache : (wbc, t) with_state
    method flush_wbc : unit -> (unit, t) Tjr_monad.m
   
    method virtual store_ops : store_ops
    method store_ops_with_cache : store_ops
    method uncached_store_ops : store_ops

    method virtual with_btree_root : (r, t) with_btree_root
    method map_ops_with_ls : map_ops_with_ls
  end
*)
