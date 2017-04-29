(* a generic kv store ------------------------------------------- *)

(*
We fix:

- disk (block_size is a parameter)
- store (constants and pp are parameters; block_size is a param because blocks/pages are typed according to size; clearly there are dependencies between these)
- map

*)

open Prelude
open Btree_api
open Page_ref_int
open Default

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
module D2S = Disk_to_store
module RS = Recycling_store
module S2M = Store_to_map


(* layers ----------------------------------------------------------- *)

(* make various layer operations eg disk_ops, store_ops, map_ops *)

let fd_ops = D.{
    get_fd=(fun () -> (fun t -> (t,Ok t.fd)));
    set_fd=(fun fd -> (fun t -> ({t with fd},Ok ())));
  }

let mk_disk_ops sz = D.make_disk sz fd_ops

let free_ops = D2S.{
    get_free=(fun () -> (fun t -> (t,Ok t.free)));
    set_free=(fun free -> (fun t -> ({t with free}, Ok ())));
  }

let mk_store_ops sz constants pp = 
  D2S.disk_to_store
    sz
    (mk_disk_ops sz)
    pp
    free_ops 

let page_ref_ops = S2M.{
    get_page_ref=(fun () -> (fun t -> (t,Ok t.root)));
    set_page_ref=(fun root -> (fun t -> ({t with root},Ok ())));
  }


(* create --------------------------------------------------------- *)

(* FIXME these aren't doing much; also, constants can be computed *)
open Pickle_params

(* we have the disk ops and block size and can take pp and compute
   constants etc; FIXME constants can be computed *)
let mk_ps1 sz constants compare_k pp : ('k,'v,'r,'t) ps1 = 
  { 
    ps0={ compare_k; constants };
    store_ops=(mk_store_ops sz constants pp) 
  }

let mk_unchecked_map_ops ps1 = 
  S2M.make_unchecked_map_ops
    ps1
    page_ref_ops

let mk_checked_map_ops ps1 r2t = 
  S2M.make_checked_map_ops 
    ps1
    r2t
    page_ref_ops


let mk_ls_ops ps1 = S2M.make_ls_ops ps1 page_ref_ops


(* root block ---------------------------------------- *)

(* we implement the map by writing the free counter and root
   page_ref into the root block *)

open Pickle
open Examples
open Btree_with_pickle

let dummy fd = { fd; free=(-1); root=(-1) }

(* FIXME expose the following pickling funs somewhere in the ops?
   as extra ops? why expose? *)
(*
let frame_to_page pp = Btree_with_pickle.frame_to_page sz pp
let page_to_frame pp = Btree_with_pickle.page_to_frame sz pp
*)
let write_root_block sz t = (
  let disk_ops = mk_disk_ops sz in
  let p : P.m = (p_pair (p_int t.free) (p_int t.root)) in
  let s = p |> P.run_w_exception "" in
  let blk = BLK.of_string sz s in
  let _ = 
    dummy t.fd 
    |> disk_ops.write 0 blk 
    |> function (_,Ok ()) -> ()
  in
  ())

let read_root_block sz fd = (
  let disk_ops = mk_disk_ops sz in
  let blk = 
    dummy fd
    |> disk_ops.read 0 
    |> function (_,Ok blk) -> blk
  in
  let u = u_pair u_int (fun _ -> u_int) in
  let (_,(free,root)) = u |> U.run_w_exception (BLK.to_string blk) in
  (free,root)
)

let from_file ~fn ~create ~init ~sz ~pp = (
  let fd = fd_from_file fn create init in
  let disk_ops = mk_disk_ops sz in
  match init with
  | true -> (
      (* now need to write the initial frame *)
      let _ = 
        let frm = Frame.Leaf_frame [] in
        let p = frm|>frame_to_page sz pp in
        dummy fd 
        |> disk_ops.write 1 p 
        |> function (_,Ok ()) -> ()
      in
      (* 0,1 are taken so 2 is free; 1 is the root of the btree *)
      let (free,root) = (2,1) in
      (* remember to write blk0 *)
      let _ = write_root_block sz {fd;free;root} in
      {fd;free;root})
  | false -> (
      let (free,root) = read_root_block sz fd in 
      {fd; free; root})
)


module type S = sig
  type k
  type v
  val pp: (k,v) Pickle_params.t 
  val sz: int
  val compare_k: k -> k -> int
end

module Make = functor (S:S) -> struct
  open S

  let cs sz = Constants.make_constants sz Btree_with_pickle.tag_len pp.k_len pp.v_len

  let mk_ps1 sz = mk_ps1 sz (cs sz) compare_k pp

  let mk_unchecked_map_ops sz = mk_unchecked_map_ops (mk_ps1 sz)

  let r2t sz = Isa_util.mk_r2t (mk_store_ops sz (cs sz) pp)

  let mk_checked_map_ops sz = mk_checked_map_ops (mk_ps1 sz)

  let ls_ops = mk_ls_ops (mk_ps1 sz)

end


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
