(* bytestore2: store arbitrary byte buffers on top of a block store ---- *)

(* 

We want to store file-like structures using an underlying B-tree
datastructure. Call this file-like structure a "bfile" just to 
distinguish from normal files.

We have already a map from index to blk. 

Now make a map from index to blk*sz, where sz <= blk_sz is the number
of bytes in the blk that form part of the data. sz is always blk_sz
apart from the last block that comprises the bfile.

We want to provide the following interface (but wrapped in a monad).
The t argument is the pointer to the file-like structure.

- pwrite: src_buffer -> src_pos -> src_len -> dst_file -> dst_offset -> int 
  - write UPTO src_len bytes from src buffer at src_pos to dst at dst_offset; 
    return number of bytes written

- pread: src_file -> src_offset -> src_len -> dst_buffer -> dst_offset -> int 
  - read UPTO count from src_offset into dst buffer at dst_offset; return number 
    of bytes read

The monad handles the B-tree side of things, so we have something
like:

- pwrite: src_buffer, src_pos, src_len, dst_pos, dst_file_root -> (int,dst_file_root') m
- pread: src_file_root, src_pos, src_len, dst_buffer, dst_pos -> int m

*)

open Btree_api
open Base_types
open Prelude
open Block

module type S = sig

  type bfile

  type blk_index = int
  type buffer = Bytes.t
  type b_off = int  (* buffer offset *)
  type count = int
  type f_off = int (* offset for the "file-like" structure *)
  type blk = Block.blk

  val blk_to_buffer: blk -> buffer
  val buffer_to_blk: buffer -> blk (* only called on result of blk_to_buffer *)

  (* a map from blk_index to blk *)
  type w

  (* the int is always <= blk_sz, so typically takes 16 bits to store at most *)
  val map_ops : (blk_index,blk*int,w) map_ops

  val blit: src:buffer -> src_pos:b_off -> dst:buffer -> dst_pos:b_off -> 
    len:int -> unit

  open Page_ref_int
  (* the file is represented by a root pointer, which is a monadic ref *)
  val root: (page_ref,w) mref

end


(* We need some support for calculating with byte ranges. *)
open Byte_range
module Rng = struct
  include Byte_range'

  (* index of a particular map *)
  type index = One | Two

  type rng = index range'
end

module Make = functor (S:S) -> struct
  
  module S = S

  open Monad

  open S


  let blk_sz = 4096

  (* calculate blk_index, offset within block and len st. offset+len
     <= blk_sz *)
  let calc ~pos ~len kk = (
    (* which block are we interested in? *)
    let blk_index = pos / blk_sz in

    (* what offset within the block? *)
    let offset = pos - (blk_index * blk_sz) in

    (* how many bytes should we read/write? *)
    let len = 
      if offset+len > blk_sz then blk_sz - offset else len in
    let _ =  assert (offset + len <= blk_sz) in
    
    kk ~blk_index ~offset ~len
  )

  (* dst_root is updated in the monad *)
  let pwrite ~src ~src_pos ~src_len ~dst_pos : (int,w) m = (
    assert (src_pos + src_len <= Bytes.length src);

    calc ~pos:dst_pos ~len:src_len (fun ~blk_index ~offset ~len:src_len -> 

        (* we can only write individual blocks to the lower layer... *)
        (* we want to avoid reading the backing block if possible *)
        let ignore_backing_block = offset=0 && src_len=blk_sz in
        match ignore_backing_block with
        | true -> (
            (* just write to block *)
          )
        | false -> (


          )




        (* backing data in the file *)
        let rng1 = Rng.{arr=One; start=0; end_=blk_sz; shift=0} in

        (* data in the buffer *)
        let rng2 = Rng.{arr=Two; start=offset; end_=offset+src_len; shift=0 } in
        
        (* merge *)
        let rng = Rng.merge rng1 rng2 in
        
        (* since s1 <= s2 and e2 <= e1, we know a bit more about the
           result: we are in case B or C or E or F *)


        let blk' = (
          match (offset<>0 || src_len < blk_sz) with
          | true -> (
              map_ops.find blk_index |> bind (fun x -> 
                  match x with 
                  | None -> return (BlkN.of_string blk_sz "")
                  | Some blk -> return blk))
          | false -> (return (BlkN.of_string blk_sz "")))
        in
        blk' |> bind (fun blk' -> 
            let blk' = blk_to_buffer blk' in

            (* blk' is the block that we are preparing; at this point, if
               needed it contains the data that was already present at
               blk_index; now we blit the new bytes *)

            (* FIXME we probably want to ensure that blks are
               immutable, but at the same time minimize copying *)

            blit ~src:src ~src_pos:src_pos ~dst:blk' ~dst_pos:offset ~len:src_len;
            let blk' = buffer_to_blk blk' in
            map_ops.insert blk_index blk' |> bind (fun _ -> 

                (* we may have to adjust the size of the bfile; but
                   clearly we want to avoid reading and writing the
                   meta block if we can; *)
                failwith "FIXME" |> bind (fun _ -> return src_len)))))


  let pread ~src_pos ~src_len ~(dst:buffer) ~dst_pos : (int,w) m = (
    assert (dst_pos + src_len <= Bytes.length dst);

    calc ~pos:src_pos ~len:src_len (fun ~blk_index ~offset ~len:src_len -> 
        map_ops.find blk_index |> bind (fun x ->
            let blk = 
              match x with 
              | None -> BlkN.of_string blk_sz "" 
              | Some x -> x
            in
            (* TODO if we are reading beyond the end, do we adjust
               file len? *)

            (* we have the blk; now copy the bytes to dst *)
            blk |> blk_to_buffer 
            |> fun src -> (
              blit ~src ~src_pos:offset ~dst ~dst_pos ~len:src_len;
              return src_len))))



end



(*
module type DISK_T = sig
  module Buffer: BUFFER
  open Buffer
  type store

  type 'a m = ('a,store) Base_types.m

  type block
  type blk_index = int

  val block_size: int 

  (* write block_size bytes from buff, unless at end of buff, in
     which case write the remainder *)
  val pwrite: buffer:buffer -> count:count -> offset:offset -> blk_index m

  val read_buff: buffer:buffer -> offset:offset -> blk_index -> unit m
end
*)


(*
module type BTREE = sig
  module Disk : DISK_T
  type ref_t = int (* typically a blk_index; FIXME exposed to make debugging easier *)
  open Disk
  val empty_btree: unit -> ref_t m
  val insert: blk_index (* k *) -> blk_index (* v *) -> ref_t -> ref_t m
  val find: ref_t -> blk_index -> blk_index option m
end
*)



(*

module type S = sig 
  (* in-memory buffer *)
  module Buff: BUFFER
  module Disk: DISK_T with module Buff = Buff

  (* copy from in-mem buffer to block; expect start to be block
     aligned? expect end to be start+block_size unless last part of
     buf? FIXME this is actually expected to mutate the block, but
     isn't in the monad (which is really concerned with the store);
     dont' use "original" block after this op! *)
  (*
  val copy: Buff.t -> int (* start *) -> int (* end *) 
    -> Disk.block -> Disk.block
     *)
  module Btree: BTREE with module Disk = Disk
end

*)


(*
  let write_buff : Buff.t -> Btree.ref_t m = (
    fun buf -> 
      (* create an empty btree *)
      Btree.empty_btree () |> bind (
        fun (r:Btree.ref_t) -> 
          (* let _ = Printf.printf "bytestore: write_buff: %d \n" r in *)
          (* allocate first block, and write length *)
          let len = Buff.length buf in
          (* insert len to metablock *)
          insert meta_key len r |> bind (
            fun r ->
              (* let _ = print_endline "bytestore 132" in *)
              (* now write out other blocks *)
              let rec f: blk_index -> ref_t -> ref_t m = (
                fun n r -> (
                    (* if len=4096, we write 1 block; if 4097, 2; if 0, 0 *)
                    let off = n * block_size in
                    match off < len with
                    | true -> (
                        (* alloc, write out, update btree, and recurse *)
                        Disk.write_buff buf off |> bind (
                          fun blk_index ->
                            insert n blk_index r |> bind (
                              fun r -> f (n+1) r)))
                    | false -> (return r)
                  ))
              in
              f 0 r
          )))
*)

(*
  let read_buff : Btree.ref_t -> Buff.t m = (
    fun r -> 
      (* get blk_index corresponding to meta block and determine length *)
      find r meta_key |> bind (
        fun (len:blk_index option) -> 
          match len with
          | None -> err __LOC__
          | Some len -> (
              (* allocate buffer *)
              let buf = Buff.create len in
              (* now read the blocks and update the buf *)
              let rec f: blk_index -> unit m = (
                fun n ->
                  let off = n*block_size in
                  match off < len with
                  | true -> (find r n |> bind (
                      fun blk_index ->
                        match blk_index with
                        | None -> (err __LOC__)
                        | Some blk_index -> 
                          Disk.read_buff buf off blk_index |> bind (
                            fun () -> f (n+1))))
                  | false -> (return ()))
              in
              f 0 |> bind (fun () -> return buf))))
*)
