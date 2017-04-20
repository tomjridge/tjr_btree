(* a generic kv store, uncached ------------------------------- *)

open Prelude
open Btree_api


(*

* Layers

| Cached | Recycling | Uncached |                      |
| X      |           |          | high-level map cache |
| X      | X         | X        | map                  |
| X      | X         |          | recycling_store      |
| X      | X         | X        | store                |
| X      |           |          | disk cache           |
| X      | X         | X        | disk                 |
| X      | X         | X        | fd                   |

TODO: cached

*)

type ('k,'v) params = {
  pp:('k,'v) Pickle_params.t;
  compare_k: 'k -> 'k -> int;
  equal_v:'v -> 'v -> bool;
}

module Make_layers = functor (S:sig type t end) -> (struct
    module S = S
    module W = struct
      type t = S.t
      type 'a m = ('a,t) Simple_monad.m
      let bind: ('a -> 'b m) -> 'a m -> 'b m = Simple_monad.bind
      let return: 'a -> 'a m = Simple_monad.return
    end

    open W

    module BA_ = Btree_api.Make(W)
    open BA_

    let page_size = 4096

    module DFD = Disk_on_fd.Make(Disk)

    module D2S = Disk_to_store.Make(struct
        module W = W
        module Disk = Disk
        module Store = Store
      end)

    module S2RS = Recycling_store.Make(struct
        module Store = Store
        (* operations from lower store *)
        type ('k,'v) lower_ops = {
          (* allocate without writing *)
          ops: ('k,'v) ops;
          store_alloc_page_ref : unit -> page_ref m;
          store_write_frame: page_ref -> ('k,'v) frame -> unit m;
        }

        (* we provide extra operations store_sync which flushes to lower *)
        type ('k,'v) rs_ops = {
          ops: ('k,'v) ops; 
          store_sync: unit -> unit m;
        }
      end)

    module S2M = Store_to_map.Make(struct
        module W = W
        module Store = Store
        module Map = Map
      end)

end)

module Make_uncached = functor (S:sig type k type v val ps: (k,v) params end) -> (struct

    module S = S
    open S

    (* make types ----------------------------------------------------- *)

    module T = struct
      type t = {
        fd: Disk_on_fd.fd;
        free: page_ref;
        root: page_ref; (* pointer to root of btree *)
      }        
    end
    include T
    module L = Make_layers (struct type t = T.t end)

    open L.BA_

    let page_size = L.page_size

    (* make various layer operations eg disk_ops, store_ops, map_ops *)

    let fd_ops = L.DFD.{
      get_fd=(fun () -> (fun t -> (t,Ok t.fd)));
      set_fd=(fun fd -> (fun t -> ({t with fd},Ok ())));
    }
    let disk_ops : Disk.ops = L.DFD.make_disk page_size fd_ops

    let free_ops = L.D2S.{
      get_free=(fun () -> (fun t -> (t,Ok t.free)));
      set_free=(fun free -> (fun t -> ({t with free}, Ok ())));
    }
    let store_ops = 
      L.D2S.disk_to_store 
        page_size 
        disk_ops 
        ps.pp 
        free_ops 
        ps.compare_k
        ps.equal_v

    let page_ref_ops = L.S2M.{
      get_page_ref=(fun () -> (fun t -> (t,Ok t.root)));
      set_page_ref=(fun root -> (fun t -> ({t with root},Ok ())));
    }
    let map_ops = L.S2M.make page_ref_ops store_ops


    (* create --------------------------------------------------------- *)

    (* we implement the map by writing the free counter and root
       page_ref into the root block *)

    open Pickle
    open Examples

    let dummy fd = { fd; free=(-1); root=(-1) }

    (* FIXME expose the following pickling funs somewhere in the ops?
       as extra ops? *)
    let sz = disk_ops.block_size
    let frame_to_page = Btree_with_pickle.frame_to_page sz ps.pp
    let page_to_frame = Btree_with_pickle.page_to_frame sz ps.pp

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
