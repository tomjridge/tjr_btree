(** Most recent interface, this time based on layers *)

open Btree_intf

(** Assume we are already provided with an Isa_btree instance *)

(* $(PIPE2SH("""sed -n '/type[ ].*btree_factory/,/^>/p' >GEN.btree_factory.ml_""")) *)
type ('k,'v,'r,'t,'leaf,'node,'dnode,'ls,'blk,'wbc) btree_factory = <
  (* monad_ops, k_cmp, cs given *)
  leaf_ops: ('k,'v,'leaf)leaf_ops;
  node_ops: ('k,'r,'node)node_ops;

  wbc_factory : ('r,'dnode,'wbc)wbc_factory;

  dnode_mshlr : blk_sz -> ('dnode, 'blk) dnode_mshlr;


  (* Store layer *)

  make_store_ops:
    blk_dev_ops : ('r, 'blk, 't) blk_dev_ops -> 
    blk_alloc   : ('r, 't) blk_allocator_ops -> 
    <
      uncached_store_ops : ('r,'dnode,'t)store_ops;

      with_wbc : 
        (* with_btree_root : ('r,'t)with_state ->  *)
        wbc_ops         : ('r,'dnode,'wbc)wbc_ops ->
        with_wbc        : ('wbc,'t)with_state ->
        <
          flush_wbc          : unit -> (unit,'t)m;
          store_ops_with_wbc : ('r,'dnode,'t)store_ops
        >;
    >;

  (* Upper layers *)

  pre_btree_ops: 
    ('r,'dnode,'t)store_ops -> 
    ('k, 'v, 'r, 't, 'leaf, 'node, 'ls) pre_btree_ops;

  map_ops_with_ls: 
    pre_btree_ops   : ('k, 'v, 'r, 't, 'leaf, 'node, 'ls) pre_btree_ops ->
    with_btree_root : ('r,'t)with_state -> 
    ('k,'v,'r,'ls,'t) map_ops_with_ls;


  (* Convenience *)

  uncached:
    blk_dev_ops     : ('r, 'blk, 't) blk_dev_ops -> 
    blk_alloc       : ('r, 't) blk_allocator_ops -> 
    init_btree_root : 'r -> 
    ('k,'v,'r,'ls,'t) map_ops_with_ls;

  cached:
    blk_dev_ops     : ('r, 'blk, 't) blk_dev_ops -> 
    blk_alloc       : ('r, 't) blk_allocator_ops -> 
    wbc_params      : wbc_params ->  
    init_btree_root : 'r -> 
    <
      flush_wbc       : unit -> (unit,'t)m;
      (* sync_key        : 'k -> (unit,'t)m; *)
      map_ops_with_ls : ('k,'v,'r,'ls,'t) map_ops_with_ls;
    >; 
>

(* $(PIPE2SH("""sed -n '/^module type S/,/end/p' >GEN.S.ml_""")) *)
module type S = sig
  type k
  type v
  type r
  type t
  val k_cmp     : k -> k -> int
  val monad_ops : t monad_ops
  val cs        : Constants.constants

  val k_mshlr   : k bp_mshlr
  val v_mshlr   : v bp_mshlr
  val r_mshlr   : r bp_mshlr

  val r_cmp     : r -> r -> int (* for wbc *)
end

module type T = sig
  type k
  type v
  type r
  type t
  type node
  type leaf
  type ls
  type blk = ba_buf
  type wbc

  val btree_factory : (k,v,r,t,leaf,node,(node,leaf)dnode,ls,blk,wbc) btree_factory
end

(** Make with unrestricted sig *)
module Make_v1(S:S) = struct
  open S

  type k = S.k
  type v = S.v
  type r = S.r
  type t = S.t
  type blk = ba_buf

  module M1 = Make_1.Make(S)

  type node = M1.node
  type leaf = M1.leaf
  type ls = M1.leaf_stream

  module W_ = Write_back_cache.Make
      (struct type t = r let compare = S.r_cmp end)
      (struct type t = (node,leaf)Isa_btree.dnode end)

  type wbc = W_.wbc

  type nonrec node_cnvs = (k,v,r,node,leaf)node_cnvs

  let ( >>= ) = S.monad_ops.bind
  let return = S.monad_ops.return

  let leaf_ops = M1.leaf_ops
  let node_ops = M1.node_ops
  let wbc_factory = W_.wbc_factory

  let dnode_mshlr blk_sz = 
    let (k,v,r) = (k_mshlr,v_mshlr,r_mshlr) in
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
        let blk_sz = blk_sz
        type nonrec node = node
        type nonrec leaf = leaf
        let node_cnvs = M1.node_cnvs
      end

      module Y = Bin_prot_marshalling.Make(X)
    end)
    in      
    Y.dnode_mshlr


  let make_store_ops 
      ~(blk_dev_ops : ('r, 'blk, 't) blk_dev_ops)  
      ~(blk_alloc   : ('r, 't) blk_allocator_ops)
    =
    let dnode_mshlr = dnode_mshlr blk_dev_ops.blk_sz in
    let uncached_store_ops =
        M1.disk_to_store 
          ~disk_ops:{ dnode_mshlr; blk_dev_ops; blk_alloc }
    in
    object
      method uncached_store_ops = uncached_store_ops
      method with_wbc = fun
        ~(wbc_ops  : ('r,'dnode,'wbc)wbc_ops)
        ~(with_wbc : ('wbc,'t)with_state)
        -> 
          let wbc_evict = fun writes -> 
            (* these writes are dnodes; we need to marshall them first *)
            writes |> List.map (fun (blk_id,dn) -> 
                (blk_id,dnode_mshlr.dnode_to_blk dn))
            |> blk_dev_ops.write_many
          in
          let flush_wbc = fun () -> 
            with_wbc.with_state (fun ~state ~set_state -> 
                wbc_ops.Write_back_cache.clean state |> fun (evictees,state) -> 
                wbc_evict evictees >>= fun () -> 
                set_state state)
          in          
          let store_ops_with_wbc : (_,_,_)store_ops = 
            Store_write_back_cache.add_write_back_cache_to_store
              ~monad_ops
              ~uncached_store_ops
              ~alloc:blk_alloc.blk_alloc
              ~evict:wbc_evict
              ~write_back_cache_ops:wbc_ops
              ~with_write_back_cache:with_wbc
          in
          object
            method flush_wbc=flush_wbc
            method store_ops_with_wbc=store_ops_with_wbc
          end
      
    end
  
  let pre_btree_ops store_ops = M1.store_to_pre_btree ~store_ops

  let map_ops_with_ls ~pre_btree_ops ~with_btree_root = 
    M1.pre_btree_to_map ~pre_btree_ops ~root_ops:with_btree_root

  let uncached = 
    fun ~(blk_dev_ops:(_,_,_)blk_dev_ops) ~blk_alloc ~init_btree_root ->
    let dnode_mshlr = dnode_mshlr blk_dev_ops.blk_sz in
    let ref_ = ref init_btree_root in
    let with_btree_root = Tjr_monad.with_imperative_ref ~monad_ops ref_ in
    make_store_ops ~blk_dev_ops ~blk_alloc |> fun x -> 
    x#uncached_store_ops |> fun store_ops ->
    pre_btree_ops store_ops |> fun pre_btree_ops -> 
    map_ops_with_ls ~pre_btree_ops ~with_btree_root
                                              
  let cached = 
    fun ~(blk_dev_ops:(_,_,_)blk_dev_ops) ~blk_alloc ~(wbc_params:wbc_params) ~init_btree_root ->
    let dnode_mshlr = dnode_mshlr blk_dev_ops.blk_sz in
    let ref_ = ref init_btree_root in
    let wbc = wbc_factory#make_wbc ~cap:wbc_params#cap ~delta:wbc_params#delta in
    let wbc_ref = ref wbc#empty in
    let with_wbc = Tjr_monad.with_imperative_ref ~monad_ops wbc_ref in
    let wbc_ops = wbc#ops in
    let with_btree_root = Tjr_monad.with_imperative_ref ~monad_ops ref_ in
    make_store_ops ~blk_dev_ops ~blk_alloc |> fun x -> 
    x#with_wbc ~wbc_ops ~with_wbc |> fun y ->
    pre_btree_ops y#store_ops_with_wbc |> fun pre_btree_ops -> 
    map_ops_with_ls ~pre_btree_ops ~with_btree_root |> fun map_ops_with_ls -> 
    object
      method flush_wbc=y#flush_wbc
      method map_ops_with_ls=map_ops_with_ls
    end
 
  let btree_factory : (_,_,_,_,_,_,_,_,_,_)btree_factory = object
    method leaf_ops = leaf_ops
    method node_ops = node_ops
    method wbc_factory = wbc_factory
    method dnode_mshlr = dnode_mshlr
    method make_store_ops = make_store_ops
    method pre_btree_ops = pre_btree_ops
    method map_ops_with_ls = map_ops_with_ls
    method uncached = uncached
    method cached = cached
  end

end


(** Make with restricted sig *)
module Make_v2(S:S) : T with type k=S.k and type v=S.v and type r=S.r and type t=S.t = struct
  include Make_v1(S)
end


(** {2 Examples} *)

module Examples = struct
  let blk_sz = Blk_sz.blk_sz_4096

  module Int_int = Make_v2(struct
      type k = int
      type v = int
      type r = Shared_ctxt.r
      type t = Shared_ctxt.t
      let k_cmp: k -> k -> int = Int_.compare
      let monad_ops = Shared_ctxt.monad_ops
      let k_mshlr = bp_mshlrs#int_mshlr
      let v_mshlr = bp_mshlrs#int_mshlr
      let r_mshlr = bp_mshlrs#r_mshlr

      let k_size = let module X = (val k_mshlr) in X.max_sz
      let v_size = let module X = (val v_mshlr) in X.max_sz
      let cs = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size
      let r_cmp = Shared_ctxt.r_cmp
    end)

  let int_int_factory = Int_int.btree_factory

  module Int_r = Make_v2(struct
      type k = int
      type v = Shared_ctxt.r
      type r = Shared_ctxt.r
      type t = Shared_ctxt.t
      let k_cmp: k -> k -> int = Int_.compare
      let monad_ops = Shared_ctxt.monad_ops
      let k_mshlr = bp_mshlrs#int_mshlr
      let v_mshlr = bp_mshlrs#r_mshlr
      let r_mshlr = bp_mshlrs#r_mshlr

      let k_size = let module X = (val k_mshlr) in X.max_sz
      let v_size = let module X = (val v_mshlr) in X.max_sz
      let cs = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size
      let r_cmp = Shared_ctxt.r_cmp
    end)

  let int_r_factory = Int_r.btree_factory


end

