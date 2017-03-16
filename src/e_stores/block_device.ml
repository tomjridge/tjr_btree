(* Things related to block devices *)

(* FIXME put everything in monads, so that we can easily compose things *)

(* FIXME make a config modules, which contains basic config params - default blocksize; how many bytes to store an int etc *)

(* open Sexplib.Std (* for ppx_assert *) *)


(* basic type for in-mem block, and on-disk block ref -------------------- *)

(* FIXME move to config? *)
(*
module Defaults = struct

  (* page of block? at this level we prefer block; in mem we use page *)
  type block = Btree_api.Simple.page 

  let page_size = 4096 (* bytes *)

  (* block ref *)
  type block_ref = Btree_api.Simple.page_ref[@@deriving yojson]

  (* to make an empty block before writing to disk *)
  let empty () = String.make page_size (Char.chr 0) 

end
*)

(* a block device backed by a file ---------------------------------------- *)

module Blkdev_on_fd (* : BLOCK_DEVICE *) = struct
  open Btree_api
  module Block = Mk_block(struct let block_size=4096 end)
  type fd = Unix.file_descr
  type t = fd
  type r = Block.id
  type blk = Block.t

  type 'a m = ('a,t) Sem.m

  (* FIXME we probably want this to be a dynamic value rather than
     fixed in module *)
  let block_size (t:t) = Block.sz


(*
  let string_to_blk = BP.string_to_blk
*)
      
  let safely = Sem.safely
  let return = Sem.return
  let bind = Sem.bind

  let get_fd : unit -> fd m = fun () -> (fun s -> (s,Ok s))

  let read : r -> blk m = (
      fun r -> 
        safely __LOC__ (
          get_fd ()
          |> bind Unix.(fun fd ->
              ignore (lseek fd (r * Block.sz) SEEK_SET);
              let buf = Bytes.make Block.sz (Char.chr 0) in 
              let n = read fd buf 0 Block.sz in
              (* assert (n=Block.sz); we allow the file to expand automatically, so no reason to read any bytes *)
              assert(n=0 || n=Block.sz);
              return buf)))


  let write: r -> blk -> unit m = (
    fun r buf -> 
      safely __LOC__ (
        get_fd ()
        |> bind Unix.(fun fd ->
          ignore (lseek fd (r * Block.sz) SEEK_SET);
          let n = single_write fd buf 0 Block.sz in
          assert (n=Block.sz);
          return ())))


  let sync : unit -> unit m = ExtUnixSpecific.(fun () -> 
      safely __LOC__ (
        get_fd () |> bind (fun fd -> ExtUnixSpecific.fsync fd; return ())))

  let from_file ~fn ~create ~init = Unix.(
      let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
      openfile fn flgs 0o640 |> 
      (fun fd -> (if init then ftruncate fd 0 else ()) |> (fun _ -> fd))
    )

end

let _ = (module Blkdev_on_fd : Btree_api.BLOCK_DEVICE)



