(* a generic kv store, uncached ------------------------------- *)

open Prelude
open Btree_api
open Page_ref_int

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

let page_size = 4096

(* the global state *)
module T = struct
  type t = {
    fd: Disk_on_fd.fd;
    free: page_ref;
    root: page_ref; (* pointer to root of btree *)
  }        
end
include T

module D = Disk_on_fd
module DS = Disk_to_store
module RS = Recycling_store
module SM = Store_to_map


(* layers ----------------------------------------------------------- *)

(* make various layer operations eg disk_ops, store_ops, map_ops *)

let fd_ops = D.{
    get_fd=(fun () -> (fun t -> (t,Ok t.fd)));
    set_fd=(fun fd -> (fun t -> ({t with fd},Ok ())));
  }

let disk_ops = D.make_disk page_size fd_ops

let free_ops = DS.{
    get_free=(fun () -> (fun t -> (t,Ok t.free)));
    set_free=(fun free -> (fun t -> ({t with free}, Ok ())));
  }

let mk_store_ops pp = 
  DS.disk_to_store
    page_size 
    disk_ops 
    pp 
    free_ops 

let page_ref_ops = SM.{
    get_page_ref=(fun () -> (fun t -> (t,Ok t.root)));
    set_page_ref=(fun root -> (fun t -> ({t with root},Ok ())));
  }

(* construct ps1 *)

open Isa_util.Params_

let mk_unchecked_map_ops ps1 = 
  SM.make_unchecked_map_ops 
    ps1
    page_ref_ops

let mk_ps1 constants compare_k pp : ('k,'v,'r,'t) ps1 = 
  let s = mk_store_ops pp in
  let (store_read,store_free,store_alloc) = 
    (s.store_read,s.store_free,s.store_alloc)
  in
  {
    ps0={ compare_k; constants };
    store_read;store_free; store_alloc
  }


let mk_ls_ops ps1 = 
  SM.make_ls_ops ps1 page_ref_ops

(* create --------------------------------------------------------- *)

(* we implement the map by writing the free counter and root
   page_ref into the root block *)

open Pickle
open Examples

let dummy fd = { fd; free=(-1); root=(-1) }

(* FIXME expose the following pickling funs somewhere in the ops?
   as extra ops? why expose? *)
let sz = disk_ops.block_size
let frame_to_page pp = Btree_with_pickle.frame_to_page sz pp
let page_to_frame pp = Btree_with_pickle.page_to_frame sz pp

let write_root_block t = (
  let p : P.m = (p_pair (p_int t.free) (p_int t.root)) in
  let s = p |> P.run_w_exception "" in
  let blk = Default_block.of_string disk_ops.block_size s in
  let _ = 
    disk_ops.write 0 blk |> (fun f -> f (dummy t.fd)) 
    |> function | (_,Ok ()) -> ()
  in
  ())

let read_root_block fd = (
  let blk = 
    disk_ops.read 0 |> (fun f -> f (dummy fd)) 
    |> function (_,Ok blk) -> blk
  in
  let u = u_pair u_int (fun _ -> u_int) in
  let (_,(free,root)) = u |> U.run_w_exception (BLK.to_string blk) in
  (free,root)
)

let from_file ~fn ~create ~init ~pp = (
  let fd = fd_from_file fn create init in
  match init with
  | true -> (
      (* now need to write the initial frame *)
      let _ = 
        let frm = Frame.Leaf_frame [] in
        let p = frm|>frame_to_page pp in
        disk_ops.write 1 p |> (fun f -> f (dummy fd))
        |> function (_,Ok ()) -> ()
      in
      (* 0,1 are taken so 2 is free; 1 is the root of the btree *)
      let (free,root) = (2,1) in
      (* remember to write blk0 *)
      let _ = write_root_block {fd;free;root} in
      {fd;free;root})
  | false -> (
      let (free,root) = read_root_block fd in 
      {fd; free; root})
)



(* TODO a high-level cache over Insert_many -------------------------------------- *)

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
