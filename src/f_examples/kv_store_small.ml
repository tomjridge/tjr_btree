(* a small KV store; keys and values are <=256 bytes *)

(* we store the btree generation in block 0 *)


open Btree_api
open Map_string_string_small
open Block_device
open File_store

module RF = File_store.Recycling_filestore

module MSS = Map_string_string_small.Make(RF)
module Btree_ = MSS.Simple.Btree
module KV = Map_string_string_small.KV

module FS = Filestore

open KV
open Btree_api

type t = {store: RF.store; page_ref:RF.page_ref}

let lens = Lens.({ 
    from=(fun t -> (t.store,t)); to_=(fun (s',t) -> {t with store=s'})})

let sync t = (
  (* we want to write page_ref into block 0 *)
  let (blk,None) =  Pickle.(Examples.p_int t.page_ref |> P.run "") in
  let Ok blk = Blkdev_on_fd.Block.string_to_blk blk in
  let fd = t.store.RF.fs.fd in
  Blkdev_on_fd.write 0 blk |> Sem.run fd 
  |> function (_,Ok ()) -> 
    RF.sync () |> Sem.run t.store 
    |> function (store,Ok ()) -> {t with store=store}
)

(* initialize *)
let init' = RF.(
    lift (FS.set_free 0) |> Sem.bind (fun () -> 
        alloc_block () |> Sem.bind (fun r ->
            assert (r=0);
            Btree_.Raw_map.empty () 
          )))

let from_file ~fn ~create ~init = (
  assert (not create || init); (* create --> init *)
  let fd = Blkdev_on_fd.from_file ~fn ~create ~init in
  let fs = Filestore.from_fd ~fd ~init in
  let store = RF.from_filestore fs in
  let t = (
    match init with
    | true -> (
        init' |> Sem.run store 
        |> function (store,Ok page_ref) -> {store;page_ref})
    | false -> (
        (* read from block 0 *)
        RF.read 0 |> Sem.bind (fun blk -> 
            Pickle.Examples.u_int |> Sem.run blk
            |> (fun (_,Ok i) -> Sem.return i))
        |> Sem.run store |> function (store,Ok page_ref) -> {store;page_ref})
  )
  in
  t)


(* FIXME above is horrible *)
let main args = (
  (* turn off wf checking *)
  Test.disable ();
  match args with
  | ["init"; fn] -> (
      from_file ~fn ~create:true ~init:true 
      |> (fun t -> 
          (sync t |> fun t -> ());
          print_endline "init ok";
          ()
        ))
  | ["insert";fn;k;v] -> (
      from_file ~fn ~create:false ~init:false
      |> (fun t -> 
          Btree_.Raw_map.insert (SS.from_string k) (SS.from_string v) 
          |> Sem.run (t.store,t.page_ref) 
          |> (function ((store,page_ref),Ok()) -> 
              ignore (sync {store;page_ref}));
          (* print_endline "insert ok"; *)
        )
    )
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false 
      |> (fun t -> 
          Btree_.Leaf_stream_.mk t.page_ref |> Sem.run t.store
          |> function (store,Ok ls) -> 
            Btree_.Leaf_stream_.all_kvs () |> Sem.run (t.store,ls) 
            |> function (_,Ok kvs) -> 
              (List.iter (fun (k,v) -> 
                   Printf.printf "%s -> %s\n" (SS.to_string k) (SS.to_string v)) kvs);
              print_endline "list ok"))
  | _ -> (failwith ("Unrecognized args: "^
                   (Tjr_string.concat_strings " " args)^
                    __LOC__))
)







(*
type ops_t = { 
  insert: key -> value -> unit;
  insert_many: key -> value -> (key*value) list -> unit;
  find: key -> value option;
  delete: key -> unit;
  sync: unit -> unit;
  mk_leaf_stream: unit -> (key,value) imperative_leaf_stream_t;
}
*)

(*
let mk: Filestore.store -> ops_t = (
  fun store ->
    let (store,page_ref) = (
      match store.free_ref with
      | 0 -> (
          (* empty store *)
          let store = store |> RF.filestore_to_recycling_filestore in
          (* reserve first block *)
          let (store,Ok r) = RF.alloc_block () |> Sem.run store in          
          let (store,page_ref) = 
            MSS.Simple.Btree.Raw_map.empty |> Sem.run store
            |> (fun (s',Ok r) -> (s',r))
          in
          (store,page_ref))
      | _ -> (
          (* store is not empty; retrieve btree ref from block 0 *)
          Blkdev_on_fd.read 0 |> Sem.run store.fd
          |> (fun (s',Ok blk) -> 
              Pickle.Examples.u_int |> Sem.run blk
              |> (fun (_,Ok i) ->
                  (store|>RF.filestore_to_recycling_filestore,i)))))
    in
    let store = ref store in
    let page_ref = ref page_ref in
    let ops = IM.mk store page_ref in
    let sync = (fun () -> 
        RF.sync |> Sem.run !store 
        |> (fun (s',res) -> 
            store := s';
            (* write btree ref into block 0 *)
            let (blk,None) = Pickle.(Examples.p_int (!page_ref) |> P.run "") in
            let blk = 
              blk ^ (String.make (Blkdev_on_fd.block_size - String.length blk) (Char.chr 0)) 
            in 
            Blkdev_on_fd.write 0 blk |> Sem.run (!store).fs.fd 
            |> function (s',Ok ()) -> (
                (* only fd "changes" but since it is a ref, no need to do anything *)
                ()
              )
          ))  (* FIXME what about res? *)
    in
    let mk_leaf_stream () = 
      MSS.Simple.Btree.Imperative_leaf_stream.mk (!store) (!page_ref) 
      |> (fun (_,x) -> x)
    in
    {insert=ops.insert; insert_many=ops.insert_many;find=ops.find; 
     delete=ops.delete; sync; mk_leaf_stream }
)


let mk initialize fn = (
  (match (Tjr_prelude.File.exists fn) with
   | true -> Unix.(openfile fn [O_RDWR] 0o640)
   | false -> (Unix.openfile fn [O_RDWR;O_CREAT] 0o640))
  |> Filestore.(
      if initialize then mk_fd_to_empty_store else mk_fd_to_nonempty_store)
  |> mk)
*)


(* module type IM' = IMPERATIVE_MAP with module KV=KV *)


(* type t = RF.store *)
