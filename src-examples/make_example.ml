(** Various examples, using lwt, bin_prot and bigarray *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Tjr_monad.With_lwt

module Make(S: sig
    type blk_id = Blk_id_as_int.blk_id  [@@deriving bin_io]
    type r      = blk_id                [@@deriving bin_io]
    type blk    = ba_buf
    type k                              [@@deriving bin_io]
    type v                              [@@deriving bin_io]
    val k_cmp : k -> k -> int
    val r_cmp : r -> r -> int (* needed for write_back_cache *)
    val cs    : constants
  end)
= 
struct
  open S

  let r_cmp = r_cmp

  let blk_sz = blk_sz_4096

  let monad_ops = lwt_monad_ops
  
  module Btree = Tjr_btree.Make(
    struct
      include S
      type t = lwt
      let monad_ops = lwt_monad_ops
    end)
  open Btree

  module W_ = Write_back_cache.Make_write_back_cache
      (struct type t = r let compare = r_cmp end)
      (struct type t = (node,leaf)dnode end)
  type write_back_cache = W_.Internal.Lru.t
  let make_write_back_cache = W_.make_write_back_cache
  
  module X_ = Bin_prot_marshalling.Make(
    struct
      include S
      include Btree
      let node_cnvs = node_cnvs
      let blk_sz = blk_sz
    end)
  let dnode_mshlr = X_.dnode_mshlr

  let blk_ops = Blk_factory.make_3 ()
      
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
  (* NOTE reserve block 0 for the system root block *)
  let initial_bt_rt_blkid = 1
  let root_ops : (r,lwt) btree_root_ops = {
    with_state=(fun f -> 
        f 
          ~state:(
            assert(!bt_rt_ref >= 0);
            !bt_rt_ref|> Blk_id_as_int.of_int)
          ~set_state:(fun blk_id -> bt_rt_ref:=Blk_id_as_int.to_int blk_id; return ()))
  }

  module Root_blk = struct
    open Bin_prot.Std
    type t = {
      bt_rt:int;
      blk_alloc:int
    }[@@deriving bin_io]
    let get_roots () = { bt_rt=(!bt_rt_ref); blk_alloc=(!blk_alloc_ref) }
    let set_roots x = 
      bt_rt_ref := x.bt_rt;
      blk_alloc_ref:= x.blk_alloc
    let to_blk x = 
      let blk = Bigstring.make 4096 chr0 in
      let _n = bin_write_t blk ~pos:0 x in
      blk       
    let from_blk blk = 
      let x = bin_read_t blk ~pos_ref:(ref 0) in
      x
  end

  module Make_2(U: sig
      val blk_dev_ops : (r,blk,lwt) blk_dev_ops
    end)
  = 
  struct
    open U
    (* at this point, we have the blk_dev_ops *)
    let disk_ops = { dnode_mshlr; blk_dev_ops; blk_alloc }

    let b0 = (Blk_id_as_int.of_int 0)

    let read_root_block () = 
      blk_dev_ops.read ~blk_id:b0

    let init_refs_from_root_block () = 
      read_root_block () >>= fun blk ->
      Root_blk.(from_blk blk |> set_roots);
      return ()

    let write_root_block blk = 
      blk_dev_ops.write ~blk_id:b0 ~blk

    let write_refs_to_root_block () = 
      Root_blk.(get_roots () |> to_blk) |> fun blk ->
      write_root_block blk

    (* NOTE this creates a blk_dev with no kvs *)
    let initialize_blk_dev () =
      blk_dev_ops.write 
        ~blk_id:(Blk_id_as_int.of_int initial_bt_rt_blkid) 
        ~blk:(empty_leaf_as_blk ()) >>= fun () ->
      bt_rt_ref:=initial_bt_rt_blkid; 
      (* we assume the next blk is free *)
      blk_alloc_ref:=initial_bt_rt_blkid+1;
      write_refs_to_root_block () >>= fun () ->
      return ()

    let evict writes = 
      (* these writes are dnodes; we need to marshall them first *)
      writes |> List.map (fun (blk_id,dn) -> 
          (blk_id,dnode_mshlr.dnode_to_blk dn))
      |> blk_dev_ops.write_many

    let store_ops = Btree.disk_to_store ~disk_ops

    (* initial state and ops *)
    let cache = make_write_back_cache ~cap:5200 ~delta:10

    let cache_ref = ref cache.initial_state

    let with_cache = {
      with_state = fun f -> f ~state:!cache_ref ~set_state:(fun s -> cache_ref:=s; return ())
    }

    let store_ops = 
      Store_cache.add_write_back_cache_to_store 
        ~monad_ops 
        ~store_ops 
        ~alloc:blk_alloc.blk_alloc
        ~evict
        ~write_back_cache_ops:cache.ops
        ~with_write_back_cache:with_cache
        
    (* make sure to call before close *)
    let flush_cache () = 
      cache.ops.trim_all (!cache_ref) |> fun (writes,_) ->
      evict writes

    let pre_btree_ops = Btree.store_to_pre_btree ~store_ops

    let map_ops_with_ls = Btree.pre_btree_to_map ~pre_btree_ops ~root_ops
  end
end

