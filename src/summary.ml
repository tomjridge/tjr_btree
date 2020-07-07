(** [Make_6] main interfaces:

{[
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

type ('k,'v,'r,'t,'leaf,'node,'dnode,'ls,'blk,'wbc) btree_factory = <
  (* monad_ops, k_cmp, cs given *)
  leaf_ops: ('k,'v,'leaf)leaf_ops;
  node_ops: ('k,'r,'node)node_ops;

  wbc_factory : ('r,'dnode,'wbc)wbc_factory;

  dnode_mshlr : blk_sz -> ('dnode, 'blk) dnode_mshlr;

  write_empty_leaf: 
    blk_dev_ops : ('r, 'blk, 't) blk_dev_ops -> 
    blk_id : 'r -> 
    (unit,'t)m;

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
    < 
      get_btree_root  : unit -> ('r,'t)m;
      map_ops_with_ls : ('k,'v,'r,'ls,'t) map_ops_with_ls
    >;
  
  cached:
    blk_dev_ops     : ('r, 'blk, 't) blk_dev_ops -> 
    blk_alloc       : ('r, 't) blk_allocator_ops -> 
    wbc_params      : wbc_params ->  
    init_btree_root : 'r -> 
    <
      get_btree_root  : unit -> ('r,'t)m;
      flush_wbc       : unit -> (unit,'t)m;
      (* sync_key        : 'k -> (unit,'t)m; *)
      map_ops_with_ls : ('k,'v,'r,'ls,'t) map_ops_with_ls;
    >; 
>

type ('dnode,'blk) dnode_mshlr = {
  dnode_to_blk : 'dnode -> 'blk;
  blk_to_dnode : 'blk -> 'dnode;
  blk_sz       : blk_sz
}

  type ('k,'v,'r,'ls,'t) map_ops_with_ls = {
    find            : k:'k -> ('v option, 't)m;
    insert          : k:'k -> v:'v -> (unit,'t) m;
    delete          : k:'k -> (unit,'t)m;
    insert_many     : k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m;
    insert_all      : kvs:('k * 'v) list -> (unit, 't) m;    
    leaf_stream_ops : ('k,'v,'r,'ls,'t)Isa_btree_intf.leaf_stream_ops;
  }

]}

*)
