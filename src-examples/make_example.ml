(** Construct an example API, using lwt, bin_prot and bigarray *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Tjr_monad.With_lwt
open Intf_


module Make(S:S) : (EX with type k=S.k and type v=S.v and type t=lwt) = (struct
  (* open S *)

  type t = lwt
  (* type nonrec flg=flg *)

  (** following aliases make the documentation more concise *)
  type ba_buf = Tjr_fs_shared.ba_buf
  type lwt = Tjr_monad.With_lwt.lwt 
  type r' = Blk_id_as_int.blk_id [@@deriving bin_io]

  module S2 = struct
    include S
    type blk_id = r'                     [@@deriving bin_io]
    type r      = blk_id                [@@deriving bin_io]
    type blk    = ba_buf

    let r_cmp : r -> r -> int = Pervasives.compare (* needed for write_back_cache *)
    type t = lwt
    let monad_ops = lwt_monad_ops
  end
  open S2
  type r = S2.r
  type k = S2.k
  type v = S2.v

  let monad_ops = lwt_monad_ops

  module B = Blk_id_as_int

  module Blk = struct
    let blk_sz = blk_sz_4096
    (* let blk_ops = Blk_factory.make_3 () *)
  end
  open Blk
  
  module Btree = Tjr_btree.Make(S2)
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

  module W_ = Write_back_cache.Make_write_back_cache
      (struct type t = r let compare = r_cmp end)
      (struct type t = dnode end)
  type write_back_cache = W_.Internal.Lru.t
  (* let make_write_back_cache = W_.make_write_back_cache *)
  let {initial_state=init_cache;ops=cache_ops} = W_.make_write_back_cache ~cap:5200 ~delta:10
  let _ = init_cache
  let _ = cache_ops  

  let with_ref r = 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
    { with_state }

  (** Run-time state of the example; container for fd, and various refs *)
  type bd = {
    fd: Lwt_unix.file_descr;
    blk_dev_ops: (r,ba_buf,lwt) blk_dev_ops;
    blk_alloc_ref: int ref;
    (* blk_alloc_ops: (r,lwt) blk_allocator_ops; this can be a function from t *)
    bt_rt_ref: int ref;
    (* root_ops: (r,lwt) btree_root_ops; *)
    cache_ref: write_back_cache ref;
  }

  let blk_alloc (t:bd) : (r,lwt) blk_allocator_ops = 
    let blk_alloc_ref = t.blk_alloc_ref in
    {
      blk_alloc=(fun () -> 
        assert(!blk_alloc_ref>=0);
        (* Printf.printf "Allocating blk: %d\n" (!blk_alloc_ref); *)
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

    let b0 = B.of_int 0 (* where we store the "superblock" *)
    let b1 = 1 (* where we store the initial empty btree leaf node *)
    let b2 = 2 (* first free blk *)

    let read_root_block t = t.blk_dev_ops.read ~blk_id:b0

    let write_root_block t blk = t.blk_dev_ops.write ~blk_id:b0 ~blk

    let sync_from_disk t = 
      read_root_block t >>= fun blk ->
      (from_blk blk |> set_roots t);
      return ()

    let sync_to_disk ~bd:t = 
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
      sync_to_disk ~bd:t >>= fun () ->
      return ()
  end

  let sync_to_disk,sync_from_disk = Root_blk.(sync_to_disk,sync_from_disk)

  let disk_ops t = { dnode_mshlr; blk_dev_ops=t.blk_dev_ops; blk_alloc=(blk_alloc t) }

  let open_ ?flgs:(flgs=[]) fn = 
    Blk_dev_factory.(make_6 (Filename fn)) >>= fun x -> 
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
    (match List.mem O_TRUNC flgs with
     | true -> Root_blk.initialize_blk_dev t
     | false -> return ()) >>= fun () ->
    (* NOTE we always attempt to sync back from the root blk, even if
       we have already done this with initialize_blk_dev *)
    sync_from_disk t >>= fun () -> 
    (match List.mem O_NOCACHE flgs with 
     | true -> (Printf.printf "Warning: O_NOCACHE not implemented"; return ())
     | false -> return ()) >>= fun () ->
    return t

  let evict t writes = 
    (* these writes are dnodes; we need to marshall them first *)
    writes |> List.map (fun (blk_id,dn) -> 
        (blk_id,dnode_mshlr.dnode_to_blk dn))
    |> t.blk_dev_ops.write_many

  (* FIXME remove this unsafe use of magic *)
         (*
  let k_to_int k = 
    assert(debug_k_and_v_are_int);
    let k_to_int : k -> int = Obj.magic in
    k_to_int k
*)

  (*
  let v_to_int v = 
    assert(debug_k_and_v_are_int);
    let v_to_int : v -> int = Obj.magic in
    v_to_int v
*)
(*
  module Pvt = struct
    type ('a,'b) dn = Dnode of 'a | Dleaf of 'b [@@deriving show]
    type cache_bindings = (int * ((int list * int list, (int * int) list) dn * bool)) list[@@deriving show]
  end
  open Pvt
*)
  (* (int * ((k list * int list, (k * v) list) dn * bool)) list *)
(*
  let show_cache t = 
    let c = (!(t.cache_ref)) in
    cache_ops.bindings c |> fun xs -> 
    xs |> List.map (fun (r,(dn,dirty)) -> 
        r |> B.to_int |> fun r -> 
        dn |> (function 
          | Disk_node n -> Dnode (
              n |> node_cnvs.node_to_krs |> fun (ks,rs) -> 
              (ks |> List.map k_to_int, List.map B.to_int rs))
          | Disk_leaf l -> Dleaf (l |> node_cnvs.leaf_to_kvs
                                  |> List.map (fun (k,v) -> (k_to_int k, v_to_int v))))
        |> fun dn -> 
        (r,(dn,dirty))) |> fun xs ->
    Printf.printf "Cache is: %s\n%!" (show_cache_bindings xs); 
    return ()
*)
  let show_cache _t = 
    (* Printf.printf "Call to show_cache\n%!"; *)
    return ()

  (* make sure to call before close *)
  let flush_cache ~bd:t = 
    show_cache t >>= fun () ->
    (* Printf.printf "Cache size: %d\n%!" (cache_ops.size (!(t.cache_ref))); *)
    cache_ops.trim_all (!(t.cache_ref)) |> fun (writes,_cache) ->
    assert(cache_ops.size _cache=0);
    evict t writes >>= fun () ->
    (* FIXME we want to use the original cache, but updated so that no
       entries are dirty; this probably needs a modification to the
       LRU interface https://github.com/pqwy/lru/issues/7 ; wait for
       new lru version then replace the following *)
    let c = !(t.cache_ref) in
    (writes,c) |> iter_k (fun ~k (ws,c) -> 
        match ws with
        | [] -> c
        | (r,dn)::ws -> 
          W_.Internal.Lru.add r (dn,false) c |> fun c -> 
          k (ws,c)) |> fun c -> 
    t.cache_ref := c; 
    (* the following shouldn't be necessary if we have serialized access to t.fd *)
    (* from_lwt (Lwt_unix.fsync t.fd) >>= fun () -> *)
    return ()

  let map_ops_with_ls t : (k,v,r,Btree.leaf_stream,lwt) map_ops_with_ls = 
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
            ~set_state:(fun blk_id ->
              let blk_id = B.to_int blk_id in
              (* Printf.printf "Updating bt_rt to %d\n%!" blk_id; *)
              bt_rt_ref:=blk_id; return ()))
      }
    in
    let uncached_store_ops = Btree.disk_to_store ~disk_ops:(disk_ops t) in
    (* add cache *)
    let store_ops = 
      Store_write_back_cache.add_write_back_cache_to_store
        ~monad_ops 
        ~uncached_store_ops
        ~alloc:(blk_alloc t).blk_alloc
        ~evict:(evict t)
        ~write_back_cache_ops:cache_ops
        ~with_write_back_cache:(with_cache t)
    in
    let pre_btree_ops = Btree.store_to_pre_btree ~store_ops in
    let map_ops_with_ls = Btree.pre_btree_to_map ~pre_btree_ops ~root_ops:(root_ops t) in
    map_ops_with_ls

  module Pub = struct
    let find ~bd:t = (map_ops_with_ls t).find
    let insert ~bd:t = (map_ops_with_ls t).insert
    let insert_many ~bd:t = (map_ops_with_ls t).insert_many
    let insert_all ~bd:t = (map_ops_with_ls t).insert_all
    let delete ~bd:t = (map_ops_with_ls t).delete
    let ls_create ~bd:t = 
      let r = !(t.bt_rt_ref) in
      (map_ops_with_ls t).leaf_stream_ops.make_leaf_stream (B.of_int r)
    (* FIXME ls_step and ls_kvs are independent of t *)
    let ls_step ~bd:t ~ls:lss = (map_ops_with_ls t).leaf_stream_ops.ls_step lss 
    let ls_kvs ~bd:t ~ls:lss = (map_ops_with_ls t).leaf_stream_ops.ls_kvs lss 
  end
  include Pub

  let close ~bd:t =
    flush_cache ~bd:t >>= fun () ->
    sync_to_disk ~bd:t >>= fun () ->
    from_lwt (Lwt_unix.close t.fd)
end)

