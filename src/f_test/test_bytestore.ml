(* implement and test bytestore ---------------------------------------- *)

(* backing store is in memory;  *)

open Bytestore
open Btree_util

module Params = struct
  type page = string (* 4096 *)
  let page_size = 4096

  type block = string
  type blk_id = int
end

module Buff = struct
  type t = bytes (* we want to read into a buf; different from block/page *)
  let length = Bytes.length
  let create = (fun n -> Bytes.make n (Char.chr 0))
end

let _ = (module Buff : Buff_t)

(* FIXME here we want to have an in-mem store to page *)

(* in memory disk for testing ---------------------------------------- *)

module Disk = struct
  module Buff = Buff
  type store = {free:int; map: Params.page Map_int.t}
  type store_error  (* no constructors *)
  let empty_disk = {free=0; map=Map_int.empty}

  type 'a m = ('a,store) Btree_api.Sem.m  

  type block = Params.block (* 4096 *)
  type blk_id = Params.blk_id
  let block_size = Params.page_size

  (* write at most block_size bytes from buf at position off; returns
     id of block containing write *)
  let write_buff: Buff.t -> offset -> blk_id m = (
    fun buf off s -> 
      (* get at most block_size bytes from buf, pad to block_size, and update store *)
      let len = 
        let x = Buff.length buf - off in
        if x < block_size then x else block_size
      in
      let page = Bytes.sub_string buf off len ^ (String.make (block_size - len) '\000') in
      let page_id = s.free in
      ({free=s.free+1; map=Map_int.add page_id page s.map},Ok page_id)
  )

  (* read at most block_size bytes into buf at offset off from blk_id *)
  let read_buff: Buff.t -> offset -> blk_id -> unit m = (
    fun buf off i s -> 
      let len = 
        let x = Bytes.length buf - off in
        if x>block_size then block_size else x
      in
      let page = Map_int.find i s.map in
      let _ = Bytes.blit_string page 0 buf off len in
      (s,Ok ())
  )

  (* additional Btree.STORE interface -------------------------------------- *)
  type page = string
  type page_ref = int[@@deriving yojson]

  let alloc : page -> page_ref m = (
    fun p s -> 
      ({free=s.free+1; map=Map_int.add s.free p s.map},Ok s.free))

  let dest_Store : store -> page_ref -> page = (
    fun s r -> 
      (* print_endline (string_of_int r);  *)
      try (Map_int.find r s.map) with _ -> failwith "dest_Store" )

  let page_ref_to_page: page_ref -> page m = (
    fun r s -> 
      try (s,Ok(Map_int.find r s.map)) with _ -> failwith "page_ref_to_page")

  let free : page_ref list -> unit m = (
    fun rs s -> (s,Ok ()))

  let page_size = Params.page_size

end

let _ = (module Disk: Disk_t)

let _ = (module Disk: Btree_api.STORE)


(* btree backed by Disk ---------------------------------------- *)

module Btree' (* : Ext_bytestore.Btree_t *) = struct 

  open Btree_api
  module Disk = Disk
  type ref_t = int

  module Int_int_store = Map_int_int.Make(Disk)
  

  open Disk
  open Int_int_store.Btree_simple.Btree

  let empty_btree: unit -> ref_t m = (
    fun () -> Raw_map.empty ()
  )

  let insert: blk_index (* k *) -> blk_id (* v *) -> ref_t -> ref_t m = (
    fun k v r -> (
        fun s ->
          Raw_map.insert k v |> Sem.run (s,r) |> (fun ((s',r'),res) -> 
              match res with
              | Ok () -> (s',Ok r')
              | Error e -> (s',Error e)
            )
      ))

  let find: ref_t -> blk_index -> blk_id option m = (
    fun r k -> (
        fun s ->
          Raw_map.find k |> Sem.run (s,r) |> (fun ((s',r'),res) ->
              match res with
              | Ok v -> (s',Ok v)  (* assume r' = r *)
              | Error e -> (s',Error e))))

end


let _ = (module Btree': Bytestore.Btree_t)


(* instantiate Bytestore ---------------------------------------- *)

module Bytestore' = Bytestore.Make(struct
    module Buff=Buff
    module Disk=Disk
    module Btree=Btree'
end)


(* do some tests ---------------------------------------- *)

(* write and read back various length strings *)

open Disk
open Btree_api.Sem
open Btree_api

let test len = (
  let buf = Bytes.make len 'a' in
  let r = (Bytestore'.write_buff buf)
          |> bind (fun r -> 
              (* Printf.printf "write_buff: ref1 %d\n" r; *)
          Bytestore'.read_buff r)
          |> bind (fun buf' ->
          assert (Bytes.to_string buf' = Bytes.to_string buf);
          print_string ".";
          return ())    
  in
  r |> Sem.run Disk.empty_disk 
)

let main () = 
  Printf.printf "%s: " __MODULE__;
  ignore (test 1);
  let _ = List.map test [0;1;4095;4096;4097;8191;8192;8193;40000] in ();
  print_newline ();
  ()

