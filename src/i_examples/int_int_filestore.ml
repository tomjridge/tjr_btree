(* a map from int to int backed by file store ------------------------------- *)
open Prelude
open Btree_api

(*

* Layers

| Cached | Uncached |                      | 
| X      |          | high-level map cache | 
| X      | X        | map                  | 
| X      |          | recycling_store      | 
| X      | X        | store                | 
| X      |          | disk cache           | 
| X      | X        | disk                 | 
| X      | X        | fd                   | 

TODO: cached

*)

module Uncached = (struct

  (* make types ----------------------------------------------------- *)

  module W = struct
    type t = {
      fd: Disk_on_fd.fd;
      free: page_ref;
      root: page_ref; (* pointer to root of btree *)
    }
    type 'a m = ('a,t) Simple_monad.m
    let bind: ('a -> 'b m) -> 'a m -> 'b m = Simple_monad.bind
    let return: 'a -> 'a m = Simple_monad.return
  end

  let _ = (module W : WORLD)
  open W

  module Disk = struct
    module W = W
    type ops = {
      block_size: BLK.sz;
      read: BLK.r -> BLK.t m;
      write: BLK.r -> BLK.t -> unit m;
      disk_sync: unit -> unit m;
    }
  end

  let _ = (module Disk : DISK)

  let page_size = 4096

  module TMPD = struct
    module DFD = Disk_on_fd.Make(Disk)
    open DFD
    let fd_ops = {
      get_fd=(fun () -> (fun t -> (t,Ok t.fd)));
      set_fd=(fun fd -> (fun t -> ({t with fd},Ok ())));
    }
    let disk_ops : Disk.ops = make_disk page_size fd_ops
  end

  let disk_ops = TMPD.disk_ops

  module Store = struct
    module W = W
    type ('k,'v) ops = {
      compare_k: 'k -> 'k -> int;
      equal_v: 'v -> 'v -> bool;
      cs0: constants;
      store_free: page_ref list -> unit m;
      store_read : page_ref -> ('k, 'v) frame m;  (* FIXME option? *)
      store_alloc : ('k, 'v) frame -> page_ref m;
      mk_r2f: t -> page_ref -> ('k,'v) frame option;
    }
  end

  let _ = (module Store : STORE)

  module TMPS = struct
    module S = struct
      module W = W
      module Disk = Disk
      module Store = Store
    end
    module D2S = Disk_to_store.Make(S)
    open D2S
    let free_ops = {
      get_free=(fun () -> (fun t -> (t,Ok t.free)));
      set_free=(fun free -> (fun t -> ({t with free}, Ok ())));
    }
    open Map_int_int
    let store_ops = 
      disk_to_store 
        page_size 
        disk_ops 
        Map_int_int.pp 
        free_ops 
        KV.key_ord 
        KV.equal_value
  end

  let store_ops = TMPS.store_ops

  module Map = struct
    module W = W
    module LS = struct 
      type ('k,'v) leaf_stream
      type ('k,'v) t = ('k,'v) leaf_stream
      type ('k,'v) ops = {
        step: ('k,'v) t -> ('k,'v) t option m;
        get_kvs: ('k,'v) t -> ('k*'v) list m
      }
    end
    type ('k,'v) ops = {
      find: 'k -> 'v option m;
      insert: 'k -> 'v -> unit m;
      delete: 'k -> unit m;
      get_leaf_stream: unit -> ('k,'v) LS.t m;
    }
  end

  let _ = (module Map : MAP)

  module TMPM = struct
    module S = struct
      module W = W
      module Store = Store
      module Map = Map
    end
    module S2M = Store_to_map.Make(S)
    open S2M
    let page_ref_ops = {
      get_page_ref=(fun () -> (fun t -> (t,Ok t.root)));
      set_page_ref=(fun root -> (fun t -> ({t with root},Ok ())));
    }
    let map_ops = S2M.make page_ref_ops store_ops
  end

  let map_ops = TMPM.map_ops


  (* create --------------------------------------------------------- *)

  (* we implement the map by writing the free counter and root
     page_ref into the root block *)

  module Create = (struct 

    open Pickle
    open Examples

    let dummy fd = { fd; free=(-1); root=(-1) }

    (* FIXME expose the following pickling funs somewhere in the ops?
       as extra ops? *)
    let pp = Map_int_int.pp
    let sz = disk_ops.block_size
    let frame_to_page = Btree_with_pickle.frame_to_page sz pp
    let page_to_frame = Btree_with_pickle.page_to_frame sz pp

    let write_root_block fd free root = (
      let p : P.m = (p_pair (p_int free) (p_int root)) in
      let s = p |> P.run_w_exception "" in
      let blk = Default_block.of_string disk_ops.block_size s in
      let _ = 
        disk_ops.write 0 blk |> (fun f -> f (dummy fd)) 
        |> function | (_,Ok ()) -> ()
      in
      ())

    let read_root_block fd = (
      let blk = 
        disk_ops.read 0 |> (fun f -> f (dummy fd)) 
        |> function (_,Ok blk) -> blk
      in
      let u = u_pair u_int (fun _ -> u_int) in
      let (_,(free,root)) = u |> U.run_w_exception "" in
      (free,root)
    )

    let from_file ~fn ~create ~init = (
      let fd = fd_from_file fn create init in
      let (free,root) = (
        match init with
        | true -> (
            (* now need to write the initial frame *)
            let _ = 
              let frm = Frame.Leaf_frame [] in
              let p = frm|>frame_to_page in
              disk_ops.write 1 p |> (fun f -> f (dummy fd))
              |> function (_,Ok ()) -> ()
            in
            (* 0,1 are taken so 2 is free; 1 is the root of the btree *)
            (2,1))
        | false -> (read_root_block fd))
      in
      {fd; free; root})
  end) (* Create *)

  let from_file = Create.from_file

end) (* Uncached *)



(* a high-level cache over Insert_many -------------------------------------- *)

(* we cache at the map level *)

(*
module Cached (* : Btree.S *) = struct
  open Btree_util
  open Uncached

  type kvs = (KV.key * KV.value) list

  type pending_inserts = int Map_int.t  (* the high-level cache *)

  type t = ST.page_ref * ST.store * pending_inserts

  type 'a m = ('a,t) Sem.m

  module Insert = struct

    (* just add to cache *)
    let insert : KV.key -> KV.value -> unit m = (
      fun k v t -> 
        let (r,s,ps) = t in
        let ps' = Map_int.add k v ps in
        ((r,s,ps'),Ok()))

  end

  open Internal_api

  (* FIXME monads a bit of a hassle :( *)
  let sync : unit -> unit m = (
    fun () t -> Sem.(
      let (r,s,kvs) = t in
      (* insert all that are in the cache, using insert_many.cache *)
      let kvs = Map_int.bindings kvs in
      match kvs with 
      | [] -> (t,Ok ())
      | (k,v)::kvs -> (
          let open Uncached.Btree_simple_internal_.Btree in
          Raw_map.insert_many k v kvs |> Sem.run (s,r) |> (fun ((s',r'),res) ->
              match res with
              | Ok () -> ((r',s',Map_int.empty),Ok())
              | Error e -> ((r',s',Map_int.empty),Error e)))))

end

*)
