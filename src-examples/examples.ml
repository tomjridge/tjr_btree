(** Various examples *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Tjr_monad.With_lwt

(* open Intf_ *)

(** The steps to construct an example are:

- fix the monad type (eg store passing)
- construct block ops {!Tjr_fs_shared.Block_ops}, which converts
  strings/bytes to/from blks
- construct block device ops {!Tjr_fs_shared.Blk_dev_ops_type}, which
  reads and writes to blks
- implement {!Bin_prot_marshalling.node_leaf_conversions} to convert
  from list-based leaf/node impls to the efficient leaf/node impls
- implement marshalling procedures via {!Bin_prot_marshalling}
- calculate constants based on blk_sz and key and value types, and
  marshalling strategy
- implement blk_allocator_ops, to allow allocation of blks via id
- for every desired combination of (key/value types, marshalling,
  blk_dev, blk_allocator), use {!Tjr_btree.disk_to_store} to
  construct a corresponding store
- then use {!Tjr_btree.store_to_map} to convert store to a map
  (using a root pointer to convert the pre_map_ops to a map_ops)

FIXME include this documentation in main tjr_btree lib, perhaps as a
simple int->int example

{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 
%}

*)


module Make(S: sig
    type blk_id = Blk_id_as_int.blk_id[@@deriving bin_io]
    type r      = blk_id[@@deriving bin_io]
    type blk    = ba_buf
    type k[@@deriving bin_io]
    type v[@@deriving bin_io]
    val k_cmp: k -> k -> int
    val cs: constants
    (* val kv_mrshlr: (k,v,ba_buf) kv_mshlr *)
  end)
= 
struct
  open S

  let blk_sz = blk_sz_4096
  
  module Btree = Tjr_btree.Make(
    struct
      include S
      type t = lwt
      let monad_ops = lwt_monad_ops
    end)
  open Btree


  module X_ = Bin_prot_marshalling.Make(
    struct
      include S
      include Btree
      let node_cnvs = node_cnvs
      let blk_sz = blk_sz
    end)
  let dnode_mshlr = X_.dnode_mshlr

  let mk_blk_dev_ops ~filename = 
    Blk_dev_factory.(make_6 (Filename filename))

  let empty_leaf_as_blk () = 
    dnode_mshlr.dnode_to_blk (Disk_leaf (node_cnvs.kvs_to_leaf []))

  let with_ref r = 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
    { with_state }

  (* in-mem min_free_blk_id *)
  let blk_alloc_ref = ref (-1)
  (* let blk_alloc = with_ref blk_alloc_ref *)
  let blk_alloc : (r,lwt) blk_allocator_ops = {
    blk_alloc=(fun () -> 
        assert(!blk_alloc_ref>=0);
        let r = !blk_alloc_ref |> Blk_id_as_int.of_int in
        incr blk_alloc_ref;
        return r);
    blk_free=(fun _ -> return ())
  }

  (* in-mem blk_id for the btree root *)
  let bt_rt_ref = ref (-1)
  let bt_rt = with_ref bt_rt_ref
  let root_ops : (r,lwt) btree_root_ops = {
    with_state=(fun f -> 
        f 
          ~state:(
            assert(!bt_rt_ref >= 0);
            !bt_rt_ref|> Blk_id_as_int.of_int)
          ~set_state:(fun blk_id -> bt_rt_ref:=Blk_id_as_int.to_int blk_id; return ()))
  }


  module Make_2(U: sig
      val blk_dev_ops : (r,blk,lwt) blk_dev_ops
    end)
  = 
  struct
    open U
    (* at this point, we have the blk_dev_ops *)
    let disk_ops = { dnode_mshlr; blk_dev_ops; blk_alloc }
    let map_ops_with_ls = Btree.disk_to_map ~disk_ops ~root_ops

    let initialize_blk_dev () =
      blk_dev_ops.write ~blk_id:(Blk_id_as_int.of_int 0) ~blk:(empty_leaf_as_blk ()) >>= fun () ->
      bt_rt_ref:=0;
      blk_alloc_ref:=0;
      return ()

  end
end


type arg = 
  | A1_int_int

module Make_1() = struct
  include Make(struct
      open Bin_prot.Std
      type blk_id = Blk_id_as_int.blk_id[@@deriving bin_io]
      type r      = blk_id[@@deriving bin_io]
      type blk    = ba_buf
      type k = int[@@deriving bin_io]
      type v = int[@@deriving bin_io]
      let k_cmp: k -> k -> int = Pervasives.compare
      let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory *)
    end)
end



(*
type ('k,'v,'r,'t,'blk_id,'blk,'blk_dev_ops,'fd,'node,'leaf,'leaf_stream,'store_ops,'wb,'dnode,'extra) example = {
  monad_ops             : 't monad_ops;
  compare_k             : 'k -> 'k -> int;
  blk_ops               : 'blk blk_ops;
  blk_dev_ops           : 'fd -> 'blk_dev_ops;
  blk_allocator_ref     : 'blk_id blk_allocator_state ref; 
  blk_allocator         : ('blk_id blk_allocator_state,'t)with_state;
  blk_allocator_ops     : ('blk_id, 't) blk_allocator_ops;
  reader_writers        : ('k,'v)reader_writers;
  nlc                   : ('k,'v,'r,'node,'leaf) nlc;
  marshalling_ops       : ('dnode,'blk) marshalling_ops;
  disk_ops              : 'fd -> ('blk_id,'t,'dnode,'blk) disk_ops;
  evict : 
    blk_dev_ops:('r, 'blk, 't) blk_dev_ops ->
    ('r * 'dnode) list -> 
    (unit, 't) m;
  make_write_back_cache : 
    cap:int -> delta:int -> 
    ('wb,('r,'dnode,'r*'dnode,'wb)write_back_cache_ops) initial_state_and_ops;
  add_write_back_cache  : 
    blk_dev_ops:'blk_dev_ops -> 
    store_ops:'store_ops -> 
    with_write_back_cache:('wb,'t)with_state -> 
    'store_ops;
  wbc_ref               : 'wb ref; 
  with_write_back_cache : ('wb,'t)with_state;
  flush_wbc             : blk_dev_ops:'blk_dev_ops -> unit -> (unit,'t)m; (* FIXME 'wb -> (unit,'t)m? *)
  store_ops             : note_cached:unit -> 'fd -> ('blk_id,'dnode,'t) store_ops;
  pre_btree_ops         : 
    note_cached:unit -> 'fd -> ('k,'v,'blk_id,'t,'leaf,'node,'leaf_stream) pre_btree_ops;
  btree_root_ref        : 'blk_id btree_root_state ref;
  btree_root_ops        : ('blk_id btree_root_state,'t)with_state;
  map_ops_with_ls       : note_cached:unit -> 'fd -> ('k,'v,'r,'leaf_stream,'t)map_ops_with_ls;
  empty_leaf_as_blk     : 'blk; 
  extra                 : 'extra
}
(* NOTE the use of refs here means we need to allocate a new ref for
   each example instance. We use refs essentially so that we have an
   easy set/get method for initialization/finalization. *)

(** Some doc about types etc *)
module Internal_doc = struct
  (* for a value of type 'a, just name as a function arg *)

  type ('k,'v,'r,'t,'node,'leaf,'leaf_stream,'dnode,'blk,'wbc) types_ = {
    (* Tjr_btree.Make; generative; note that we don't need to depend on t at this stage *)
    btree_make: 'k * 'v * 'r -> k_cmp:('k -> 'k -> int) -> cs:constants -> ('node*'leaf*'leaf_stream);

    (* simple sum type *)
    note_dnode: 'node * 'leaf -> 'dnode;  (* ('node,'leaf)dnode *)
    
    (* generative *)
    write_back_cache: 'r * 'dnode -> 'wbc;  (* actually needs ('node,'leaf)dnode *)

    (* main marshalling routine *)
    marshalling_ops: ('dnode,'blk)marshalling_ops;
  }

end
*)


(* (write_back_cache,(r,dnode,r*dnode,write_back_cache) write_back_cache_ops) *)
module Make_write_back_cache'(S:sig type r val compare: r->r->int type dnode end) = struct
  open S
  module K = struct
    type t = r
    let compare: t -> t -> int = S.compare
  end
  module V = struct
    type t = dnode
  end
  module B = Write_back_cache.Make_write_back_cache(K)(V)
  include B
  let make_write_back_cache = B.make_write_back_cache
  type write_back_cache = B.Internal.Lru.t    
end

