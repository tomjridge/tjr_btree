
(** [Make_5] main interfaces:

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

  type ('k,'v,'r,'ls,'t) bt_1 = <
    map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
  >

  type ('k,'v,'r,'ls,'t) bt_2 = <
    flush_wbc: unit -> (unit,'t)m;
    sync_key: 'k -> (unit,'t)m;
    map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
  >

  type ('k,'v,'r,'t,'ls,'blk,'dnode,'wbc) btree_factory = <
    (* method blk_dev_ops: ('r,'blk,'t) blk_dev_ops *)
    (* method blk_allocator_ops: ('r,'t)blk_allocator_ops *)
    empty_leaf_as_blk: 'blk;
    wbc_factory: ('r,'dnode,'wbc)wbc_factory;
    make_uncached: ('r, 't) with_btree_root -> ('k,'v,'r,'ls,'t) bt_1;
    make_cached_1: ('r, 't) with_btree_root -> ('wbc,'t)with_state -> ('k,'v,'r,'ls,'t) bt_2;
    make_cached_2: ('r, 't) with_btree_root -> ('k,'v,'r,'ls,'t) bt_2;
  >

  (** A collection of block-based interfaces *)
  type ('r,'t,'dnode,'blk) disk_ops = {
    dnode_mshlr : ('dnode,'blk)dnode_mshlr;
    blk_dev_ops : ('r,'blk,'t)blk_dev_ops;
    blk_alloc   : ('r,'t)blk_allocator_ops
  }

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

type ('k,'v,'r,'node,'leaf) node_cnvs = {
  node_to_krs: 'node -> 'k list * 'r list;
  krs_to_node: ('k list * 'r list) -> 'node;
  leaf_to_kvs: 'leaf -> ('k * 'v) list;
  kvs_to_leaf: ('k * 'v) list -> 'leaf;
}

]}

*)
