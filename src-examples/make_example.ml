(** Construct an example API, using lwt, bin_prot and bigarray *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Tjr_monad.With_lwt
open Intf_

module Blk_id = Blk_id_as_int
type blk_id = Blk_id.blk_id

open Tjr_fs_shared.Rt_blk

(* module Pvt : RT_BLK = Rt_blk.Make(struct open Bin_prot.Std type data = int[@@deriving bin_io] end) *)

module Pvt = struct
  
  (* documentation: what we expect to be in scope (with some dummy types) *)
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

  let _NOTE_make_1_is_the_common_part_including_marshalling_etc = ()

  module Make_1(S:S) = struct
    (* open S *)
    type t = lwt
    (* type nonrec flg=flg *)

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
      include Tjr_btree.Make(S2)
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

    module W_ = Write_back_cache.Make_write_back_cache
        (struct type t = r let compare = r_cmp end)
        (struct type t = dnode end)
    type write_back_cache = W_.Internal.Lru.t
    (* let make_write_back_cache = W_.make_write_back_cache *)
    let {initial_state=init_cache;ops=cache_ops} = W_.make_write_back_cache ~cap:5200 ~delta:10
    let _ = init_cache
    let _ = cache_ops  

    
    let b0 = Blk_id.of_int 0 (* where we store the "superblock" *)
    let b1 = Blk_id.of_int 1 (* where we store the initial empty btree leaf node *)
    let b2 = Blk_id.of_int 2 (* first free blk *)

  end


  module Pvt_make(S:S) : T with type k=S.k and type v = S.v = Make_1(S)

end


module Make(S:S) : (EX with type k=S.k and type v=S.v and type t=lwt) = (struct
  
  include Pvt.Make_1(S)
  open S2

  open Bin_prot.Std
  type rt_pair = {
    bt_rt:blk_id ref;
    blk_alloc:blk_id ref
  }[@@deriving bin_io]    

  module Rt_blk_ = struct
    module X = Rt_blk.Make(struct type data = rt_pair[@@deriving bin_io] end)
    include X
  end

  type rt_blk = Rt_blk_.rt_blk


  (** Run-time state of the example; container for fd, and various refs *)
  type bd = {
    fd: Lwt_unix.file_descr;
    blk_dev_ops: (r,ba_buf,lwt) blk_dev_ops;
    rt_blk: rt_blk;
    cache_ref: write_back_cache ref;
  }

  let blk_alloc (t:bd) : (r,lwt) blk_allocator_ops = 
    {
      blk_alloc=(fun () -> 
          (* Printf.printf "Allocating blk: %d\n" (!blk_alloc_ref); *)
          let r = t.rt_blk.data.blk_alloc in
          assert(!r |> Blk_id.to_int >=0);
          let r' = !r in
          Blk_id.incr r;
          return r');
      blk_free=(fun _ -> return ())
    }

  let disk_ops t = { dnode_mshlr; blk_dev_ops=t.blk_dev_ops; blk_alloc=(blk_alloc t) }

  let initialize_blk_dev ~fd ~(blk_dev_ops:(blk_id,blk,t)blk_dev_ops) =
    (* truncate file *)
    from_lwt (Lwt_unix.ftruncate fd 0) >>= fun () ->
    (* write empty leaf into b1, and update bt_rt and blk_alloc *)
    let rt_pair = { bt_rt=ref b1; blk_alloc=ref b2 } in
    blk_dev_ops.write 
      ~blk_id:b1
      ~blk:(empty_leaf_as_blk ()) >>= fun () ->
    (* sync rt blk *)
    Rt_blk_.write_to_disk ~blk_dev_ops ~blk_id:b0 ~data:rt_pair >>= fun () ->
    let t : rt_blk = { blk_dev_ops; blk_id=b0; data=rt_pair } in
    return t

  let open_ ?flgs:(flgs=[]) fn = 
    Blk_dev_factory.(make_6 (Filename fn)) >>= fun x -> 
    let module A = (val x) in
    let open A in (* close_blk_dev is just close on fd *)
    (match List.mem O_TRUNC flgs with
     | true -> initialize_blk_dev ~fd ~blk_dev_ops
     | false -> Rt_blk_.make ~blk_dev_ops ~blk_id:b0) >>= fun rt_blk ->
    (match List.mem O_NOCACHE flgs with 
     | true -> (Printf.printf "Warning: O_NOCACHE not implemented"; return ())
     | false -> return ()) >>= fun () ->
    return {
      fd;
      blk_dev_ops;
      rt_blk;
      cache_ref=ref init_cache
    }
      

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

  let map_ops_with_ls (t:bd) : (k,v,r,Btree.leaf_stream,lwt) map_ops_with_ls = 
    let with_cache t = with_ref t.cache_ref in
    (* NOTE reserve block 0 for the system root block *)
    let root_ops t : (r,lwt) btree_root_ops = 
      let bt_rt_ref = t.rt_blk.data.bt_rt in
      {
        with_state=(fun f -> 
            f 
              ~state:(
                assert(!bt_rt_ref |> Blk_id.to_int >= 0);
                !bt_rt_ref)
              ~set_state:(fun blk_id ->
                  (* let blk_id = B.to_int blk_id in *)
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
      let r = !(t.rt_blk.data.bt_rt) in
      (map_ops_with_ls t).leaf_stream_ops.make_leaf_stream r
    (* FIXME ls_step and ls_kvs are independent of t *)
    let ls_step ~bd:t ~ls:lss = (map_ops_with_ls t).leaf_stream_ops.ls_step lss 
    let ls_kvs ~bd:t ~ls:lss = (map_ops_with_ls t).leaf_stream_ops.ls_kvs lss 
  end
  include Pub

  let sync_to_disk ~bd = 
    flush_cache ~bd >>= fun () ->
    Rt_blk_.sync_to_disk bd.rt_blk >>= fun () ->
    return ()

  let close ~bd =
    sync_to_disk ~bd >>= fun () ->
    from_lwt (Lwt_unix.close bd.fd)
end)


