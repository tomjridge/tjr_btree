(* support file operations on top of a btree -------------------- *)

(* data is stored using an initial "metadata" block (eg recording the
   length of the file); the other blocks store the actual data

   parameters: buff (in-mem buffer); disk (with function to write buff
   at offset to a block, and monad); btree implementation (sharing
   with disk)


   provides: pread, pwrite, size, truncate

*)

open Our.Util
open Btree_api

(* type blk_index = int *)
type offset = int
type length = int
type count = int
type blk_index = int
   

FIXME separate out the (blkid -> blk) structure, then add in support for pread and pwrite (and size in bytes)

this is in bytestore? needs to hide value type block_id and replace with block option

(* effectively a map from blk_index (include -1) to Block.id option *)
module type BTREE = sig  FIXME change to BLOCK_MAP, with size in bytes
  module Block_device : BLOCK_DEVICE
  type ptr = int 
  (* typically a Block.id; FIXME exposed to make debugging easier *)
  open Block_device
  type k = blk_index
  type v = Block.id  FIXME change to blk and add a function to set and get size
  val empty_btree: unit -> ptr m
  val insert: k -> v -> ptr -> ptr m
  val find: ptr -> k -> v option m (* FIXME arg order *)
end

module type BUFFER = sig
  include BUFFER
  val sub:t -> offset -> length -> string
end

module type S = sig 
  (* in-memory buffer *)
  module Buffer: BUFFER
  module Block_device: BLOCK_DEVICE
  module Btree: BTREE with module Block_device = Block_device
end



module Make = functor (S:S) -> struct
  
  module S = S

  open S
  open Buffer
  open Block_device
  open Btree

  open Btree_api.Sem
      
  (* use this to store the length of the buffer *)
  let meta_key = -1

  type 'a m = 'a Block_device.m


  (* update relevant blocks (possibly partial bloocks) and possibly
     update the size *)
  let pwrite : Buffer.t*offset*int -> Btree.ptr*offset -> Btree.ptr m option = (
    fun (buf,off1,count) (ptr,off2) -> 
      match (off1>=0 && off2>=0 && off1+count <= Buffer.length buf) with
      | false -> None
      | true -> Some(
          Btree.find ptr meta_key |> bind (
            fun len0 ->
              match len0 with
              | None -> (err ("impossible: "^__LOC__))
              | Some len0 -> ( 
                  let rec loop (buf,off1,count) (ptr,off2) = failwith "" in
                  let blk_index = off2 / Block.sz in
                  (* do we need to read the start of the block? *)
                  let aligned = (blk_index * Block.sz = off2) in
                  (
                    match aligned with
                    | true -> (
                        (* can we write a full block? *)
                        let available = min count Block.sz in
                        match available = Block.sz with
                        | true -> return (Buffer.sub buf off1 Block.sz)
                        | false -> failwith ""
                      ) 
                    | false -> failwith "")
                  |> bind (fun s -> 
                      let Pervasives.Ok blk = Block.string_to_blk s in
                      (* do the write by updating the btree *)
                      Btree.insert blk_index blk ptr)


FIXME need to have insert etc work with blks rather than blkref to make things easier
                ))))

(*
 
      (* create an empty btree *)
      Btree_.empty_btree () |> bind (
        fun (r:Btree_.ptr) -> 
          (* let _ = Printf.printf "bytestore: write_buff: %d \n" r in *)
          (* allocate first block, and write length *)
          let len = Buffer_.length buf in
          (* insert len to metablock *)
          insert meta_key len r |> bind (
            fun r ->
              (* let _ = print_endline "bytestore 132" in *)
              (* now write out other blocks *)
              let rec f: Block.id -> ptr -> ptr m = (
                fun n r -> (
                    (* if len=4096, we write 1 block; if 4097, 2; if 0, 0 *)
                    let off = n * block_size in
                    match off < len with
                    | true -> (
                        (* alloc, write out, update btree, and recurse *)
                        Block_device.write_buff buf off |> bind (
                          fun Block.id ->
                            insert n Block.id r |> bind (
                              fun r -> f (n+1) r)))
                    | false -> (return r)
                  ))
              in
              f 0 r
          )))
*)
(* FIXME add ability to pread/pwrite at offset in another file *)

  let read_buff : Btree_.ptr -> Buffer_.t m = (
    fun r -> 
      (* get Block.id corresponding to meta block and determine length *)
      find r meta_key |> bind (
        fun (len:Block.id option) -> 
          match len with
          | None -> err __LOC__
          | Some len -> (
              (* allocate buffer *)
              let buf = Buffer_.create len in
              (* now read the blocks and update the buf *)
              let rec f: Block.id -> unit m = (
                fun n ->
                  let off = n*block_size in
                  match off < len with
                  | true -> (find r n |> bind (
                      fun Block.id ->
                        match Block.id with
                        | None -> (err __LOC__)
                        | Some Block.id -> 
                          Block_device.read_buff buf off Block.id |> bind (
                            fun () -> f (n+1))))
                  | false -> (return ()))
              in
              f 0 |> bind (fun () -> return buf))))

end
