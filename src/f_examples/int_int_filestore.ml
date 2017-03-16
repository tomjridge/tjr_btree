(* a map from int to int backed by file store ------------------------------- *)

module KV = Map_int_int.KV

(* int-int store on recycling filestore ------------------------------------- *)

(* from here we specialize to recycling_filestore *)

module ST = File_store.Recycling_filestore

module Uncached = struct

  open Btree_api
  open Block_device

  module X_ = Map_int_int.Make(ST)
  module Btree_simple_ = X_.Btree_simple
  (* module IIS_ = Int_int_store *)

  let from_file ~fn ~create ~init = (
    let open Btree_simple_.Btree.S.FT in
    let open ST in
    let fd = Blkdev_on_fd.from_file fn create init in
    (* now need to write the initial frame *)
    (* FIXME this is duplicated elsewhere *)
    let frm = Leaf_frame [] in
    let p = frm|>frame_to_page in
    let r = 0 in
    let () = (
      match Blkdev_on_fd.(write r p |> Sem.run fd) with
      | (_,Error e) -> failwith (__LOC__ ^ e)
      | _ -> ())
    in
    File_store.(
      {fs = Filestore.{fd=fd;free_ref=r+1} ;
       cache=Cache.empty;
       freed_not_synced=Btree_util.Set_int.empty;
      },r))

end

(* FIXME want this let _ = (module Int_int_filestore.Btree : Btree_api.MAP) *)


(* a high-level cache over Insert_many -------------------------------------- *)

(* we cache at the map level *)

module Cached (* : Btree.S *) = struct
  open Btree_api
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

  open Btree_api

  (* FIXME monads a bit of a hassle :( *)
  let sync : unit -> unit m = (
    fun () t -> Sem.(
      let (r,s,kvs) = t in
      (* insert all that are in the cache, using insert_many.cache *)
      let kvs = Map_int.bindings kvs in
      match kvs with 
      | [] -> (t,Ok ())
      | (k,v)::kvs -> (
          let open Uncached.Btree_simple_.Btree in
          Raw_map.insert_many k v kvs |> Sem.run (s,r) |> (fun ((s',r'),res) ->
              match res with
              | Ok () -> ((r',s',Map_int.empty),Ok())
              | Error e -> ((r',s',Map_int.empty),Error e)))))

end

