(** Another attempt; this time based on layers *)

open Btree_intf

(** Assume we are already provided with an Isa_btree instance *)
type ('k,'v,'r,'t,'leaf,'node,'dnode,'ls,'blk,'wbc) btree_factory = <
  (* monad_ops, k_cmp, cs given *)
  leaf_ops: ('k,'v,'leaf)leaf_ops;
  node_ops: ('k,'r,'node)node_ops;

  wbc_factory : ('r,'dnode,'wbc)wbc_factory;


  (* Store layer *)

  make_store_ops:
    dnode_mshlr : ('dnode, 'blk) dnode_mshlr -> 
    blk_dev_ops : ('r, 'blk, 't) blk_dev_ops -> 
    blk_alloc   : ('r, 't) blk_allocator_ops -> 
    <
      uncached_store_ops : ('r,'dnode,'t)store_ops;

      with_wbc : 
        with_btree_root : ('r,'t)with_state -> 
        wbc_ops         : ('r,'dnode,'t)wbc_ops ->
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
    dnode_mshlr     : ('dnode, 'blk) dnode_mshlr -> 
    blk_dev_ops     : ('r, 'blk, 't) blk_dev_ops -> 
    blk_alloc       : ('r, 't) blk_allocator_ops -> 
    init_btree_root : 'r -> 
    ('k,'v,'r,'ls,'t) map_ops_with_ls;

  cached:
    dnode_mshlr     : ('dnode, 'blk) dnode_mshlr -> 
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

open Make_5

module Make(S:S) = struct

  type k = S.k
  type v = S.v
  type r = S.r
  type t = S.t
  type blk = ba_buf

  module M1 = Make_1.Make(S)
  (* open M1 *)

  type node = M1.node
  type leaf = M1.leaf
  type ls = M1.leaf_stream
  (* type nonrec dnode = (node,leaf)dnode *)


  module W_ = Write_back_cache.Make
      (struct type t = r let compare = S.r_cmp end)
      (struct type t = (node,leaf)Isa_btree.dnode end)


  type wbc = W_.wbc

  (* some abbrevs *)
  (* type nonrec blk_dev_ops = (r,blk,t)blk_dev_ops *)
  type nonrec node_cnvs = (k,v,r,node,leaf)node_cnvs

  let ( >>= ) = S.monad_ops.bind
  let return = S.monad_ops.return

  let with_
      ~(dnode_mshlr : ('dnode, 'blk) dnode_mshlr) 
      ~(blk_dev_ops : ('r, 'blk, 't) blk_dev_ops)  
      ~(blk_alloc   : ('r, 't) blk_allocator_ops)
    = 
    let open (struct
      let wbc_factory = W_.wbc_factory

      let wbc_evict = fun writes -> 
        (* these writes are dnodes; we need to marshall them first *)
        writes |> List.map (fun (blk_id,dn) -> 
            (blk_id,dnode_mshlr.dnode_to_blk dn))
        |> blk_dev_ops.write_many
             
      let flush_wbc wbc_ops with_wbc = fun () -> 
        with_wbc.with_state (fun ~state ~set_state -> 
            wbc_ops.Write_back_cache.clean state |> fun (evictees,state) -> 
            wbc_evict evictees >>= fun () -> 
            set_state state)

      let with_wbc ~with_btree_root ~wbc_ops ~with_wbc = 
        let flush_wbc = flush_wbc wbc_ops with_wbc in
        let sync_key (_k:k) : (unit,t)m = failwith "FIXME" in
        let map_ops_with_ls = 
        object 
        end
    end)
    in
    object
      method wbc_factory = wbc_factory
      method uncached = failwith ""

      method with_wbc = with_wbc
        
    end

  let btree_factory : (_,_,_,_,_,_,_,_,_,_)btree_factory = object
    method leaf_ops = M1.leaf_ops
    method node_ops = M1.node_ops
    method pre_btree_ops = fun store_ops -> M1.store_to_pre_btree ~store_ops
    method with_ = with_
  end

end
