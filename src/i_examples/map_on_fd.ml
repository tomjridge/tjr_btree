(** A generic map, backed by a file descriptor *)

(*
We fix:

- disk (block_size is a parameter)
- store (constants and pp are parameters; block_size is a param because blocks/pages are typed according to size; clearly there are dependencies between these)
- map

*)

open Base_types
open Prelude
open Btree_api
open Page_ref_int
open Frame
open Params

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

module D = Disk_on_fd
module D2S = Disk_to_store
module RS = Recycling_store
module S2M = Store_to_map

module Blk = Default.Default_block

(* more common parameters ------------------------------------------- *)

let fd_ops x = x#fd_ops
let free_ops x = x#free_ops

(* layers ----------------------------------------------------------- *)

(* make various layer operations eg disk_ops, store_ops, map_ops *)

let mk_disk_ops ~ps ~fd_ops = D.make_disk ~blk_sz:(blk_sz ps) ~fd_ops

let mk_store_ops ~ps ~ops = 
  D2S.disk_to_store
    ps
    (mk_disk_ops ~ps ~fd_ops:(fd_ops ops))
    (free_ops ops)


(* create --------------------------------------------------------- *)

(* FIXME these aren't doing much; also, constants can be computed *)
open Pickle_params

let mk_map_ops ~ps ~ops = 
  mk_store_ops ~ps ~ops |> fun store_ops -> 
  S2M.store_ops_to_map_ops ~ps ~page_ref_ops:(page_ref_ops ops) ~store_ops

let mk_ls_ops ~ps ~page_ref_ops ~store_ops = 
  S2M.make_ls_ops ~ps ~page_ref_ops ~store_ops

let mk_imperative_map_ops ~ps ~ops = 
  mk_map_ops ~ps ~ops |> Btree_api.Imperative_map_ops.of_map_ops

(* root block ---------------------------------------- *)

(* we implement the map by writing the free counter and root
   page_ref into the root block *)

open Pickle
open Examples
open Btree_with_pickle.O

let write_root_block ~fd ~blk_sz ~free ~root = (
  let p : P.m = (p_pair (p_int free) (p_int root)) in
  let s = p |> P.run_w_exception "" in
  let blk_id = 0 in
  let blk = Blk.of_string blk_sz s in
  let _ = Disk_on_fd.write ~fd ~blk_sz ~blk_id ~blk in
  ())

let read_root_block ~blk_sz ~fd = (
  let blk_id = 0 in
  let blk = Disk_on_fd.read ~fd ~blk_sz ~blk_id in
  let u = u_pair u_int (fun _ -> u_int) in
  let (_,(free,root)) = u |> U.run_w_exception (Blk.to_string blk) in
  (free,root)
)

let from_file ~fn ~create ~init ~ps = (
  let blk_sz = blk_sz ps in
  let pp = pp ps in
  let fd = File_.fd_from_file fn create init in
  match init with
  | true -> (
      (* now need to write the initial frame *)
      let _ = 
        let frm = Leaf_frame [] in
        let p = frm|>frame_to_page blk_sz pp in
        Disk_on_fd.write fd blk_sz 1 p 
      in
      (* 0,1 are taken so 2 is free; 1 is the root of the btree *)
      let (free,root) = (2,1) in
      (* remember to write blk0 *)
      let _ = write_root_block ~fd ~blk_sz ~free ~root in
      (fd,free,root))
  | false -> (
      let (free,root) = read_root_block ~blk_sz ~fd in 
      (fd,free,root))
)

let close ~blk_sz ~fd ~free ~root = (
  write_root_block ~fd ~blk_sz ~free ~root;
  Unix.close fd)


module Default_implementation = struct

  (* the global state *)
  type global_state = {
    fd: Disk_on_fd.fd;
    free: page_ref;
    root: page_ref; (* pointer to root of btree *)
  }        
  type t = global_state

  let fd_ops = D.{
      get=(fun () -> (fun t -> (t,Ok t.fd)));
      set=(fun fd -> (fun t -> ({t with fd},Ok ())));
    }

  let free_ops = D2S.{
      get=(fun () -> (fun t -> (t,Ok t.free)));
      set=(fun free -> (fun t -> ({t with free}, Ok ())));
    }

  let page_ref_ops = {
    get=(fun () -> (fun t -> (t,Ok t.root)));
    set=(fun root -> (fun t -> ({t with root},Ok ())));
  }

  let ops = 
    object
      method page_ref_ops=page_ref_ops
      method free_ops=free_ops
      method fd_ops=fd_ops
    end


  (* just use ops
  let mk_disk_ops ~ps = mk_disk_ops ~ps ~fd_ops

  let mk_store_ops ~ps = mk_store_ops ~ps ~ops
      
  let mk_map_ops ~ps = mk_map_ops ~ps ~ops
      
  let mk_ls_ops ~ps = mk_ls_ops ~ps ~page_ref_ops ~store_ops:(mk_store_ops ps)

  let mk_imperative_map_ops ~ps = mk_imperative_map_ops ~ps ~ops
     *)

  let from_file ~fn ~create ~init ~ps = 
    from_file ~fn ~create ~init ~ps |> (fun (fd,free,root) -> {fd;free;root})

  let close ~blk_sz t = 
    close ~blk_sz ~fd:t.fd ~free:t.free ~root:t.root


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




