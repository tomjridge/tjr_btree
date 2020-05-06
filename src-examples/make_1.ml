(** Construct an example API, using lwt, bin_prot and bigarray and a write-back cache *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Tjr_monad.With_lwt
open Std_types
(* open Intf_ *)

(** Use this to construct constants for S if necessary *)
let make_constants ~k_size ~v_size = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size ~v_size

module type S = sig
  type k[@@deriving bin_io]
  type v[@@deriving bin_io]
  val k_cmp: k -> k -> int
  val cs: constants
end

module type T = sig
  type k
  type v
  type t
  type buf = ba_buf
  type blk = ba_buf
  type r = blk_id
  type ls
  val empty_leaf_as_blk : unit -> blk
  module type S = sig
    val flush_cache : unit -> (unit, t) Tjr_monad.m
    val map_ops_with_ls :
      (k, v, r, ls, t) Tjr_btree.Btree_intf.map_ops_with_ls
  end
  val make :
    blk_dev_ops:(r, blk, t) Tjr_fs_shared.blk_dev_ops ->
    blk_alloc:(r, t) Tjr_fs_shared.blk_allocator_ops ->
    root_ops:(r, t) Tjr_btree.Btree_intf.btree_root_ops -> (module S)
end 

module Make(S:S) : T with type k=S.k and type v=S.v and type t=lwt = struct
  type t = lwt

  [@@@warning "-34"]
  [@@@warning "-32"]

  (** following aliases make the documentation more concise *)
  type ba_buf = Tjr_fs_shared.ba_buf
  type buf = ba_buf
  type blk = ba_buf
  type lwt = Tjr_monad.With_lwt.lwt 
  type r' = Blk_id_as_int.blk_id [@@deriving bin_io]

  module S2 = struct
    include S
    type blk_id = r'                     [@@deriving bin_io]
    type r      = blk_id                [@@deriving bin_io]
    type blk    = ba_buf

    let r_cmp : r -> r -> int = Stdlib.compare (* needed for write_back_cache *)
    type t = lwt
    let monad_ops = lwt_monad_ops

  end
  open S2
  type r = S2.r[@@deriving bin_io]
  type k = S2.k
  type v = S2.v

  let monad_ops = lwt_monad_ops

  let blk_sz = blk_sz_4096

  module Btree = struct
    include Tjr_btree.Make_1.Make(S2)
    type nonrec disk_ops =
      (r, lwt, (node, leaf) Isa_btree.dnode, blk)
        Tjr_btree.Btree_intf.disk_ops
  end

  open Btree
  (* type node = Btree.node *)
  (* type leaf = Btree.leaf *)
  type leaf_stream = Btree.leaf_stream
  type ls = leaf_stream

  type nonrec dnode = (Btree.node,Btree.leaf)dnode

  module Dnode_mrshlr = Bin_prot_marshalling.Make(
    struct
      include S2
      include Btree
      let node_cnvs = node_cnvs
      let blk_sz = blk_sz
    end)
  let dnode_mshlr : (dnode,ba_buf)dnode_mshlr = Dnode_mrshlr.dnode_mshlr  (* FIXME use open (struct let dnode_mshlr = Dnode_mrshlr.dnode_mshlr end) from 4.08 *)

  let empty_leaf_as_blk () = 
    dnode_mshlr.dnode_to_blk (Disk_leaf (node_cnvs.kvs_to_leaf []))

  module W_ = Write_back_cache.Make
      (struct type t = r let compare = r_cmp end)
      (struct type t = dnode end)
  type write_back_cache = W_.wbc
  let wbc = W_.wbc_factory#make_wbc ~cap:5200 ~delta:10
  let init_cache,cache_ops = wbc#empty,wbc#ops

  let evict ~blk_dev_ops writes = 
    (* these writes are dnodes; we need to marshall them first *)
    writes |> List.map (fun (blk_id,dn) -> 
        (blk_id,dnode_mshlr.dnode_to_blk dn))
    |> blk_dev_ops.write_many

  (* $(FIXME(""" this flush code should be ported to bt/make_5""")) *)
  let flush_cache ~blk_dev_ops ~cache_ref () = 
    cache_ops.clean !cache_ref |> fun (writes,_cache) ->
    assert(cache_ops.size _cache=0);
    evict ~blk_dev_ops writes >>= fun () ->
    (* $(FIXME(""" we want to use the original cache, but updated so
       that no entries are dirty; this probably needs a modification
       to the LRU interface https://github.com/pqwy/lru/issues/7 ;
       wait for new lru version then replace the following which is
       very inefficient """)) *)
    let c = !cache_ref in
    (writes,c) |> iter_k (fun ~k (ws,c) -> 
        match ws with
        | [] -> c
        | (r,dn)::ws -> 
          cache_ops.insert r (dn,false) c |> fun c -> 
          k (ws,c)) |> fun c -> 
    cache_ref := c; 
    (* the following shouldn't be necessary if we have serialized access to t.fd *)
    (* from_lwt (Lwt_unix.fsync t.fd) >>= fun () -> *)
    return ()

  let empty_leaf_as_blk = empty_leaf_as_blk

  module type S = sig
    val flush_cache       : unit -> (unit, lwt) m
    val map_ops_with_ls   : (k, v, r, ls, lwt) Btree_intf.map_ops_with_ls
  end

  let make ~blk_dev_ops ~blk_alloc ~root_ops = 
    let module A = struct
      let empty_leaf_as_blk = empty_leaf_as_blk
      let disk_ops : disk_ops = { dnode_mshlr; blk_dev_ops; blk_alloc }
      let cache_ref = ref init_cache
      let with_cache = with_ref cache_ref
      let uncached_store_ops = Btree.disk_to_store ~disk_ops:disk_ops
      (* add cache *)
      let store_ops = 
        Tjr_btree.Pvt.Store_write_back_cache.add_write_back_cache_to_store
          ~monad_ops 
          ~uncached_store_ops
          ~alloc:blk_alloc.blk_alloc
          ~evict:(evict ~blk_dev_ops)
          ~write_back_cache_ops:cache_ops
          ~with_write_back_cache:with_cache
      let flush_cache = flush_cache ~blk_dev_ops ~cache_ref
      let pre_btree_ops = Btree.store_to_pre_btree ~store_ops
      let map_ops_with_ls = Btree.pre_btree_to_map ~pre_btree_ops ~root_ops

    end
    in
    (module A : S)    
end

(** {2 Int->int example} *)

(** k,v are both int *)
module S_int_int = struct
  open Bin_prot.Std
  type k = int[@@deriving bin_io]
  type v = int[@@deriving bin_io]
  let k_cmp: k -> k -> int = Stdlib.compare
  let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory *)
  let debug_k_and_v_are_int = true
end

module type INT_INT_EX = T with type k=int and type v=int and type t=lwt

module Int_int_ex : INT_INT_EX = Make(S_int_int)





(* module X = Make *)


(*
(* documentation: what we expect to be in scope after construction (with some dummy types) *)
module type T = sig
  (* monad_ops: lwt *)
  type k
  type v
  type r = Blk_id.blk_id
  type t = lwt
  type buf = ba_buf
  type blk = ba_buf
  val blk_sz: blk_sz

  module Btree : sig 
    type leaf 
    type node
    type leaf_stream
    type nonrec disk_ops =
      (r, lwt, (node, leaf) Isa_btree.dnode, blk)
        Tjr_btree.Btree_intf.disk_ops
    type nonrec store_ops =
      (r, (node, leaf) Isa_btree.dnode, lwt) Isa_btree.store_ops
    type nonrec pre_btree_ops =
      (k, v, r, lwt, leaf, node, leaf_stream) Isa_btree.pre_btree_ops
    val disk_to_store : disk_ops:disk_ops -> store_ops
    val pre_btree_to_map :
      pre_btree_ops:pre_btree_ops ->
      root_ops:(r, lwt) Tjr_btree.Btree_intf.btree_root_ops ->
      (k, v, r, leaf_stream, lwt)
        Tjr_btree.Btree_intf.Map_ops_with_ls.map_ops_with_ls
  end
  (* results of Btree.Make, incl disk_to_store and pre_btree_to_map *)
  (* type node *)
  (* type leaf *)
  (* type nonrec dnode = (node,leaf)dnode *)
  type dnode 
  type ls = Btree.leaf_stream
  (* val dnode_mshlr : unit *)
  val empty_leaf_as_blk: unit -> blk
  (* type bd  (\* once we have the btree descriptor/ sys descriptor *\) *)
  (* val disk_ops: bd -> (blk_id,t,dnode,blk)disk_ops *)
end
*)
