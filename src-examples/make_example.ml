(** Various examples, using lwt, bin_prot and bigarray *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Tjr_monad.With_lwt

module Make(S: sig
    type k                              [@@deriving bin_io]
    type v                              [@@deriving bin_io]
    val k_cmp : k -> k -> int
    val cs    : constants
  end)
= 
struct
  (* open S *)

  module S2 = struct
    include S
    type blk_id = Blk_id_as_int.blk_id  [@@deriving bin_io]
    type r      = blk_id                [@@deriving bin_io]
    type blk    = ba_buf

    let r_cmp : r -> r -> int = Pervasives.compare (* needed for write_back_cache *)
    type t = lwt
    let monad_ops = lwt_monad_ops
  end
  open S2

  let monad_ops = lwt_monad_ops

  let blk_sz = blk_sz_4096

  let blk_ops = Blk_factory.make_3 ()
  
  module Btree = Tjr_btree.Make(S2)
  open Btree

  module Dnode_mrshlr = Bin_prot_marshalling.Make(
    struct
      include S2
      include Btree
      let node_cnvs = node_cnvs
      let blk_sz = blk_sz
    end)
  let dnode_mshlr = Dnode_mrshlr.dnode_mshlr

  let empty_leaf_as_blk () = 
    dnode_mshlr.dnode_to_blk (Disk_leaf (node_cnvs.kvs_to_leaf []))

  module W_ = Write_back_cache.Make_write_back_cache
      (struct type t = r let compare = r_cmp end)
      (struct type t = (node,leaf)dnode end)
  type write_back_cache = W_.Internal.Lru.t
  let make_write_back_cache = W_.make_write_back_cache
  let {initial_state=init_cache;ops=cache_ops} = make_write_back_cache ~cap:5200 ~delta:10
  let _ = init_cache
  let _ = cache_ops  

  let with_ref r = 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
    { with_state }

  (* a container for fd, and various refs *)
  type t = {
    fd: Lwt_unix.file_descr;
    blk_dev_ops: (r,ba_buf,lwt) blk_dev_ops;
    blk_alloc_ref: int ref;
    (* blk_alloc_ops: (r,lwt) blk_allocator_ops; this can be a function from t *)
    bt_rt_ref: int ref;
    (* root_ops: (r,lwt) btree_root_ops; *)
    cache_ref: write_back_cache ref;
  }

  let blk_alloc (t:t) : (r,lwt) blk_allocator_ops = 
    let blk_alloc_ref = t.blk_alloc_ref in
    {
      blk_alloc=(fun () -> 
        assert(!blk_alloc_ref>=0);
        let r = !blk_alloc_ref |> Blk_id_as_int.of_int in
        incr blk_alloc_ref;
        return r);
      blk_free=(fun _ -> return ())
    }

  module Root_blk = struct
    open Bin_prot.Std
    type t = {
      bt_rt:int;
      blk_alloc:int
    }[@@deriving bin_io]
    let get_roots t = { bt_rt=(!(t.bt_rt_ref)); blk_alloc=(!(t.blk_alloc_ref)) }
    let set_roots t x = 
      t.bt_rt_ref := x.bt_rt;
      t.blk_alloc_ref:= x.blk_alloc
    let to_blk x = 
      let blk = Bigstring.make 4096 chr0 in
      let _n = bin_write_t blk ~pos:0 x in
      blk       
    let from_blk blk = 
      let x = bin_read_t blk ~pos_ref:(ref 0) in
      x

    module B = Blk_id_as_int
    let b0 = B.of_int 0 (* where we store the "superblock" *)
    let b1 = 1 (* where we store the initial empty btree leaf node *)
    let b2 = 2 (* first free blk *)

    let read_root_block t = t.blk_dev_ops.read ~blk_id:b0

    let write_root_block t blk = t.blk_dev_ops.write ~blk_id:b0 ~blk

    let sync_from_disk t = 
      read_root_block t >>= fun blk ->
      (from_blk blk |> set_roots t);
      return ()

    let sync_to_disk t = 
      (get_roots t |> to_blk) |> fun blk ->
      write_root_block t blk

    (* NOTE this creates a blk_dev with no kvs *)
    let initialize_blk_dev t =      
      from_lwt (Lwt_unix.ftruncate t.fd 0) >>= fun () ->
      t.blk_dev_ops.write 
        ~blk_id:(B.of_int b1)
        ~blk:(empty_leaf_as_blk ()) >>= fun () ->
      t.bt_rt_ref:=b1; 
      (* we assume the next blk is free *)
      t.blk_alloc_ref:=b2;
      sync_to_disk t >>= fun () ->
      return ()
  end

  let sync_to_disk,sync_from_disk = Root_blk.(sync_to_disk,sync_from_disk)

  let disk_ops t = { dnode_mshlr; blk_dev_ops=t.blk_dev_ops; blk_alloc=(blk_alloc t) }

  type open_flag = Init_empty | Init_from_b0

  (* NOTE this does not attempt to read the initial blk *)
  let open_ ~flag filename = 
    Blk_dev_factory.(make_6 (Filename filename)) >>= fun x -> 
    let module A = (val x) in
    let open A in (* close_blk_dev is just close on fd *)
    let t = {
      fd;
      blk_dev_ops=blk_dev;
      blk_alloc_ref=ref(-1);
      bt_rt_ref=ref(-1);
      cache_ref=ref init_cache
    }
    in
    match flag with
    | Init_empty -> (
        Root_blk.initialize_blk_dev t >>= fun () ->
        return t)
    | Init_from_b0 -> (
        sync_from_disk t >>= fun () ->
        return t)

  let evict t writes = 
    (* these writes are dnodes; we need to marshall them first *)
    writes |> List.map (fun (blk_id,dn) -> 
        (blk_id,dnode_mshlr.dnode_to_blk dn))
    |> t.blk_dev_ops.write_many

  (* make sure to call before close *)
  let flush_cache t = 
    cache_ops.trim_all (!(t.cache_ref)) |> fun (writes,cache) ->
    assert(cache_ops.size cache=0);
    t.cache_ref := cache;
    (* FIXME don't we need to set the cache to empty at this point? or
       at least, with all elts marked as clean*)
    evict t writes 

  let map_ops_with_ls t = 
    let with_cache t = with_ref t.cache_ref in
    (* NOTE reserve block 0 for the system root block *)
    let root_ops t : (r,lwt) btree_root_ops = 
      let bt_rt_ref = t.bt_rt_ref in
      {
        with_state=(fun f -> 
          f 
            ~state:(
              assert(!bt_rt_ref >= 0);
              !bt_rt_ref|> Blk_id_as_int.of_int)
            ~set_state:(fun blk_id -> bt_rt_ref:=Blk_id_as_int.to_int blk_id; return ()))
      }
    in
    let store_ops = Btree.disk_to_store ~disk_ops:(disk_ops t) in
    (* add cache *)
    let store_ops = 
      Store_cache.add_write_back_cache_to_store 
        ~monad_ops 
        ~store_ops 
        ~alloc:(blk_alloc t).blk_alloc
        ~evict:(evict t)
        ~write_back_cache_ops:cache_ops
        ~with_write_back_cache:(with_cache t)
    in
    let pre_btree_ops = Btree.store_to_pre_btree ~store_ops in
    let map_ops_with_ls = Btree.pre_btree_to_map ~pre_btree_ops ~root_ops:(root_ops t) in
    map_ops_with_ls

  let close t =
    flush_cache t >>= fun () ->
    sync_to_disk t >>= fun () ->
    from_lwt (Lwt_unix.close t.fd)
end

