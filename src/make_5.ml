(** Like make_1, but using bin_prot marshalling *)


[@@@warning "-33"]

open Btree_intf
(* open Btree_intf_v2 *)
open Make_1
open Write_back_cache


(** The btree factory type *)
module Btree_factory = struct

  (** uncached *)
  (* $(PIPE2SH("""sed -n '/type[ ].*bt_1/,/[ ]>/p' >GEN.bt_1.ml_""")) *)
  type ('k,'v,'r,'ls,'t) bt_1 = <
    map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
  >

  (** cached *)
  (* $(PIPE2SH("""sed -n '/type[ ].*bt_2/,/[ ]>/p' >GEN.bt_2.ml_""")) *)
  type ('k,'v,'r,'ls,'t) bt_2 = <
    flush_wbc: unit -> (unit,'t)m;
    sync_key: 'k -> (unit,'t)m;
    map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
  >

  (** factory with blk_dev_ops and blk_allocator_ops and blk_sz already provided *)
  (* $(PIPE2SH("""sed -n '/type[ ].*btree_factory/,/[ ]>/p' >GEN.btree_factory.ml_""")) *)
  type ('k,'v,'r,'t,'ls,'blk,'dnode,'wbc) btree_factory = <
    (* method blk_dev_ops: ('r,'blk,'t) blk_dev_ops *)
    (* method blk_allocator_ops: ('r,'t)blk_allocator_ops *)
    empty_leaf_as_blk: 'blk;
    wbc_factory: ('r,'dnode,'wbc)wbc_factory;
    make_uncached: ('r, 't) with_btree_root -> ('k,'v,'r,'ls,'t) bt_1;
    make_cached_1: ('r, 't) with_btree_root -> ('wbc,'t)with_state -> ('k,'v,'r,'ls,'t) bt_2;
    make_cached_2: ('r, 't) with_btree_root -> ('k,'v,'r,'ls,'t) bt_2;
  >
  (** [make_cached_2]: use an empty write_back_cache, stored in a mutable reference *)


end

(* $(PIPE2SH("""sed -n '/^module type S/,/end/p' >GEN.S.ml_""")) *)
module type S = sig
  type k
  type v
  type r
  type t
  val k_cmp: k -> k -> int
  val monad_ops: t monad_ops
  val cs: Constants.constants

  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
  val r_mshlr: r bp_mshlr

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

    method virtual k_mshlr: k bp_mshlr
    method virtual v_mshlr: v bp_mshlr
    method virtual r_mshlr: r bp_mshlr

    val pvt_d = new set_once
    method dnode_mshlr : (dnode,blk)dnode_mshlr = pvt_d#get

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

  let wbc_factory = W_.wbc_factory

  (* wbc *)
  class virtual c4 = object (self)
    inherit c3

    method wbc_o = W_.wbc_factory#make_wbc ~cap:5200 ~delta:10 

    method wbc_factory = wbc_factory

    method wbc_evict = fun writes -> 
      (* these writes are dnodes; we need to marshall them first *)
      writes |> List.map (fun (blk_id,dn) -> 
          (blk_id,self#dnode_mshlr.dnode_to_blk dn))
      |> self#blk_dev_ops.write_many        

    method flush_wbc with_write_back_cache = fun () -> 
      let (>>=) = self#monad_ops.bind in
      with_write_back_cache.with_state (fun ~state ~set_state -> 
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

    method store_ops_with_cache with_write_back_cache : store_ops = 
      Store_write_back_cache.add_write_back_cache_to_store
        ~monad_ops:S.monad_ops
        ~uncached_store_ops:self#uncached_store_ops
        ~alloc:self#blk_alloc.blk_alloc
        ~evict:self#wbc_evict
        ~write_back_cache_ops:self#wbc_o#ops
        ~with_write_back_cache

    val mutable config_cached: (wbc,t) with_state option = None
    method set_cached x = config_cached <- x

    method pre_btree_ops = store_to_pre_btree ~store_ops:(
        match config_cached with
        | None -> self#uncached_store_ops 
        | Some with_write_back_cache -> 
          self#store_ops_with_cache with_write_back_cache)
  end

  class virtual c6 = object (self)
    inherit c5
    method map_ops_with_ls with_btree_root = 
      pre_btree_to_map 
        ~pre_btree_ops:(self#pre_btree_ops)
        ~root_ops:(with_btree_root)
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

  open Btree_factory

(*
  let k_mshlr : k type_with_mshlr = failwith "FIXME"
  let v_mshlr : v type_with_mshlr = failwith "FIXME"
  let r_mshlr : r type_with_mshlr = failwith "FIXME"
*)

  let btree_factory ~(blk_dev_ops:blk_dev_ops) 
      ~(blk_allocator_ops:(r,t)blk_allocator_ops) 
      ~(blk_sz:blk_sz) 
    : (_,_,_,_,_,blk,dnode,_) btree_factory 
    = 
    let open (struct
      class c = object
        method k_mshlr = S.k_mshlr
        method v_mshlr = S.v_mshlr
        method r_mshlr = S.r_mshlr
        method blk_dev_ops = blk_dev_ops
        method blk_alloc = blk_allocator_ops
        method blk_sz = blk_sz
      end  
      class d = object
        inherit c6
        inherit c
      end
    end)
    in      
    let factory = object (self)
      method empty_leaf_as_blk = (new d)#empty_leaf_as_blk
      method wbc_factory = wbc_factory
      method make_uncached with_btree_root = object
        method map_ops_with_ls = (new d)#map_ops_with_ls with_btree_root
      end

      method make_cached_1 with_btree_root with_wbc =
        let d = new d in
        d#set_cached (Some with_wbc);
        object
          method flush_wbc = d#flush_wbc with_wbc
          method sync_key (_k:k) : (unit,t)m = failwith "FIXME"
          method map_ops_with_ls = d#map_ops_with_ls with_btree_root
        end

      method make_cached_2 with_btree_root =
        let d = new d in
        let wbc = ref (d#wbc_o#empty) in
        let with_wbc = Tjr_monad.with_imperative_ref ~monad_ops:(d#monad_ops) wbc in
        self#make_cached_1 with_btree_root with_wbc
    end
    in
    factory

  let _ :
blk_dev_ops:blk_dev_ops ->
blk_allocator_ops:(r, t) Tjr_fs_shared.blk_allocator_ops ->
blk_sz:Tjr_fs_shared.blk_sz ->
(k, v, r, t, ls, blk, dnode, wbc) btree_factory
= btree_factory

end


