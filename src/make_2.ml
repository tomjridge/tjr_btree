(** This is like make_1, but we assume binprot marshalling for
   k,v,r. This means we can simplify the interface considerably.

   NOTE that we can't subsequently insert caching at the store level
   because we have removed the store types from the interface. But we
   can have different instances which include caching or not.  *)

open Btree_intf

(** Given the relevant sizes (blocks,keys,values), construct the
   B-tree size constants *)
let make_constants = Isa_btree.Constants.mk_constants

(* FIXME merge following types with those from examples, into btree_intf *)

(** B-tree, with a write-back cache *)
class type ['k, 'v, 'r, 'ls, 't ] cached_btree = 
  object
    method map_ops     : ('k,'v,'r,'ls,'t)map_ops_with_ls
    method ls_create   : unit -> ('ls,'t)m
    method ls_step     : 'ls -> ('ls option,'t)m
    method ls_kvs      : 'ls -> ('k*'v) list
    method flush_cache : unit -> (unit,'t)m
  end 

(** B-tree, no cache (or write-through cache) *)
class type ['k, 'v, 'r, 'ls, 't ] uncached_btree = 
  object
    (* method with_bt_rt : ('r,'t)with_state *)
    method map_ops    : ('k,'v,'r,'ls,'t)map_ops_with_ls
    method ls_create  : unit -> ('ls,'t)m
    method ls_step    : 'ls -> ('ls option,'t)m
    method ls_kvs     : 'ls -> ('k*'v) list
    method empty_leaf_as_blk: unit -> ba_buf
  end 

module type Bin_mshlr = sig
  type t[@@deriving bin_io]
  val max_sz: int
end

type 'a bin_mshlr = (module Bin_mshlr with type t='a)

module type S = sig
  type k
  type v
  type r
  type t
  type blk_id = r
  type blk = ba_buf
  type buf = blk
  val monad_ops : t monad_ops
  val k_cmp     : k -> k -> int
  val blk_sz    : blk_sz
  (* val cs        : Constants.constants  (\* FIXME we shouldn't need this if we have k_mshlr withmax_sz etc*\) *)
  val k_mshlr   : k bin_mshlr
  val v_mshlr   : v bin_mshlr
  val r_mshlr   : r bin_mshlr
end

(** The result of [Make]; noticably simpler than the {!module-type:Make_1.T} version *)
module type T = sig
  type k
  type v
  type r
  type t
  type blk
  type ls


  val empty_leaf_as_blk: unit -> ba_buf

  val make_uncached_btree: 
    ?with_read_cache:bool ->
    blk_dev_ops:(r,blk,t)blk_dev_ops ->
    blk_alloc:(r,t)blk_allocator_ops ->
    root_ops:(r,t)with_state -> 
    (k,v,r,ls,t) uncached_btree
      
(*  val make_cached_btree: 
    blk_dev_ops:(r,blk,t)blk_dev_ops ->
    blk_alloc:(r,t)blk_allocator_ops ->
    root_ops:(r,t)with_state -> 
    (k,v,r,ls,t) cached_btree
FIXME TODO *)
end


module Make(S:S) 
  : T with type k=S.k and type v=S.v and type r=S.r and type t=S.t and type blk=S.blk 
= struct
  include S
      
  module K = (val k_mshlr)
  module V = (val v_mshlr)
  module R = (val r_mshlr)

  (** We convert via this datatype, so we can use (at)(at)deriving ;
      FIXME inefficient *)
  module Tree = struct
    open Bin_prot.Std
    type tree = 
      | Impossible (** this is to avoid 0 as a marshalled constructor, which hopefully identifies bugs in marshalling early *)
      | N of K.t list * R.t list 
      | L of (K.t*V.t) list [@@deriving bin_io]
  end
  open Tree

  module S' = struct
    include S
    (* NOTE this taken from bin_prot_marshalling in src-examples *)
    let make_constants ~blk_sz ~k_size ~v_size = 
      let blk_sz = blk_sz |> Blk_sz.to_int in
      let r_size = 9 in (* page_ref size in bytes; assume int63 *)
      let max_node_keys = (blk_sz - 7 - r_size) / (k_size + r_size) in
      let max_leaf_size = (blk_sz - 4) / (k_size + v_size) in
      let constants=Isa_btree.Constants.(
          {min_leaf_size=2;max_leaf_size;min_node_keys=2; max_node_keys}) in
      constants

    let cs = make_constants ~blk_sz ~k_size:K.max_sz ~v_size:V.max_sz
  end
  module Btree = Make_1.Make(S')
  open Btree
  type ls = Btree.leaf_stream

  let buf_ops = ba_buf_ops

  let dnode_mshlr = 
    let { node_to_krs; krs_to_node; leaf_to_kvs; kvs_to_leaf } = node_cnvs in
    let open Isa_btree_intf in  (* for node_ops fields *)
    let dn2tree = function
      | Disk_node n -> 
        n |> node_to_krs |> fun (ks,rs) -> 
        Test.assert_(fun () ->
          let b = 1+List.length ks = List.length rs in
          let _ : unit = if not b then Printf.printf "%s: ks rs lengths not aligned, %d %d\n%!" __LOC__ (List.length ks) (List.length rs) in
          assert(b));
        N (ks,rs)
      | Disk_leaf l -> l |> leaf_to_kvs |> fun kvs -> L kvs
    in
    let tree2dn = function
      | Impossible -> failwith ("impossible: "^__LOC__)
      | N (ks,rs) -> 
        Test.assert_(fun () -> 
            let b = 1+List.length ks = List.length rs in
            let _ : unit = 
              if not b then Printf.printf "%s: ks rs lengths not aligned, %d %d\n%!" __LOC__ (List.length ks) (List.length rs)
            in
            assert(b));
        (ks,rs) |> krs_to_node |> fun n -> Disk_node n
      | L kvs -> kvs |> kvs_to_leaf |> fun l -> Disk_leaf l
    in
    (* pull this out because frame_to_page takes an explicit blk_sz; FIXME
       should it? *)
    let blk_sz' = Blk_sz.to_int blk_sz in
    let dnode_to_blk dn = 
      let buf = buf_ops.create blk_sz' in (* NOTE not necessarily zeroed *)
      let n = bin_write_tree buf ~pos:0 (dn|>dn2tree) in
      assert(n<=blk_sz');
      buf
    in
    let blk_to_dnode blk = 
(*      let _ : unit = 
        if Debug_.debug_enabled then 
          Printf.printf "blk_to_dnode: %s...\n%!" 
            (blk |> Bigstring.to_string |> fun s -> String.sub s 0 4 |> String.escaped)
      in
*)
      let t = bin_read_tree blk ~pos_ref:(ref 0) in
      tree2dn t
    in
    ({ dnode_to_blk; blk_to_dnode; blk_sz }:('a,blk)dnode_mshlr)

  let empty_leaf_as_blk () = Btree.empty_leaf_as_blk ~dnode_to_blk:dnode_mshlr.dnode_to_blk

  let make_uncached_btree ?with_read_cache:(with_read_cache=true) 
      ~blk_dev_ops ~blk_alloc ~root_ops 
    : (_,_,_,_,_) uncached_btree 
    =
    let disk_ops = {dnode_mshlr;blk_dev_ops; blk_alloc} in
    let store_ops = Btree.disk_to_store ~disk_ops in
    (* add a read cache *)
    let store_ops = match with_read_cache with
      | true -> Store_read_cache.add_imperative_read_cache_to_store ~monad_ops ~store_ops 
      | false -> store_ops 
    in
    let pre_btree_ops = Btree.store_to_pre_btree ~store_ops in
    let map_ops = Btree.pre_btree_to_map ~pre_btree_ops ~root_ops in
    let ls_create () = root_ops.with_state (fun ~state ~set_state:_ -> 
        map_ops.leaf_stream_ops.make_leaf_stream state)
    in
    let ls_step = map_ops.leaf_stream_ops.ls_step in
    let ls_kvs = map_ops.leaf_stream_ops.ls_kvs in
    object 
      method map_ops=map_ops
      method ls_create=ls_create
      method ls_step=ls_step
      method ls_kvs=ls_kvs
      method empty_leaf_as_blk=empty_leaf_as_blk
    end
    
end

