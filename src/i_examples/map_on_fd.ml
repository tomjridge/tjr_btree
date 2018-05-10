(** A generic map, backed by a file descriptor *)

(*
We fix:

- disk (block_size is a parameter)
- store (constants and pp are parameters; block_size is a param
  because blocks/pages are typed according to size; clearly there are
  dependencies between these)
- map

*)

open Base_types
open Page_ref_int
open Frame
open Params
open Map_ops

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

open Block


(* layers: disk_ops and store_ops --------------------------------- *)

let mk_disk_ops ~monad_ops ~ps ~fd_ops = 
  D.make_disk ~monad_ops ~blk_sz:(blk_sz ps) ~fd_ops

let mk_store_ops ~monad_ops ~ps ~ops = 
  D2S.disk_to_store ~monad_ops
    ~ps
    ~disk_ops:(mk_disk_ops ~monad_ops ~ps ~fd_ops:(fd_ops ops))
    ~free_ops:(free_ops ops)


(* create --------------------------------------------------------- *)

(* FIXME these aren't doing much; also, constants can be computed *)

let mk_map_ops ~monad_ops ~ps ~ops : ('k,'v,'t) map_ops = 
  mk_store_ops ~monad_ops ~ps ~ops |> fun store_ops -> 
  S2M.store_ops_to_map_ops ~monad_ops ~constants:(constants ps) ~cmp:(cmp ps) ~page_ref_ops:(page_ref_ops ops) ~store_ops

(*
let mk_ls_ops ~monad_ops ~ps ~page_ref_ops ~store_ops = 
  Iter_leaf_stream.store_ops_to_ls_ops S2M.make_ls_ops ~ps ~page_ref_ops ~store_ops

let mk_imperative_map_ops ~ps ~ops = 
  mk_map_ops ~ps ~ops |> fun map_ops -> map_ops_to_imperative map_ops
*)

(* root block ---------------------------------------- *)

(* we implement the map by writing the free counter and root
   page_ref into the root block *)

(* TODO we use standard ocaml marshalling for the root block - this is
   a demo anyway *)

let marshal_to_string x = Marshal.to_string x []

let marshal_from_string s = Marshal.from_string s 0

let root_blk_id = 0


let write_root_block ~fd ~blk_sz ~free ~root = 
  (free,root) |> marshal_to_string |> Block.of_string blk_sz 
  |> fun blk -> Disk_on_fd.write ~fd ~blk_sz ~blk_id:root_blk_id ~blk

let read_root_block ~blk_sz ~fd = 
  Disk_on_fd.read ~fd ~blk_sz ~blk_id:root_blk_id 
  |> Block.to_string |> (fun x -> (marshal_from_string x : (int * int)))
  |> fun (free,root) -> (free,root)

let from_file ~fn ~create ~init ~ps = 
  let blk_sz = blk_sz ps in
  (*  let pp = pp ps in *)
  let fd = File_util.fd_from_file fn create init in
  match init with
  | true -> (
      (* now need to write the initial frame *)
      let _ = 
        let frm = Disk_leaf [] in
        let p = frm|>frame_to_page ps blk_sz in
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

let close ~blk_sz ~fd ~free ~root = 
  write_root_block ~fd ~blk_sz ~free ~root;
  Unix.close fd



module Default_implementation = struct

  (* the global state *)
  type global_state = {
    fd: Disk_on_fd.fd;
    free: page_ref;
    root: page_ref; (* pointer to root of btree *)
  }        
  type t = global_state

  (* at this point we can use a state-passing monad t -> t *)

  open Tjr_monad

  let monad_ops : t state_passing monad_ops = Tjr_monad.State_passing_instance.monad_ops ()

  open Tjr_monad.State_passing_instance

  let fd_ops = D.{
      get=(fun () -> with_world (fun w -> (w.fd,w)));
      set=(fun fd -> with_world (fun w -> ((),{w with fd}))); 
    }

  let free_ops = D2S.{
      get=(fun () -> with_world (fun t -> (t.free,t)));
      set=(fun free -> with_world (fun t -> ((),{t with free})));
    }

  let page_ref_ops = {
    get=(fun () -> with_world (fun t -> (t.root,t)));
    set=(fun root -> with_world (fun t -> ((),{t with root})));
  }


  let ops = 
    object
      method page_ref_ops=page_ref_ops
      method free_ops=free_ops
      method fd_ops=fd_ops
    end


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




