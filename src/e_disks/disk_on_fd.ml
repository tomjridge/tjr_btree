(* Disk on a file-descriptor *)

open Prelude
open Btree_api

type fd = Unix.file_descr
type t = fd

module Make = functor (DSK:DISK) -> struct
  module DSK = DSK

  (* now implement the DSK.ops type *)
  open DSK
  open DSK.W

  type fd_ops = {
    get_fd: unit -> fd m;
    set_fd: fd -> unit m;
  }

  let safely : string -> 'a m -> 'a m = (
    fun msg m ->
    fun s -> 
      try m s 
      with e -> (s,Error (msg ^ (Printexc.to_string e))))

  let make_disk block_size ops = (
    let read: BLK.r -> BLK.t m = (fun r ->
        safely __LOC__ (
          ops.get_fd ()
          |> bind Unix.(fun fd ->
              ignore (lseek fd (r * block_size) SEEK_SET);
              let buf = Bytes.make block_size (Char.chr 0) in 
              let n = read fd buf 0 block_size in
              (* assert (n=block_size); we allow the file to expand
                 automatically, so no reason to read any bytes *)
              assert(n=0 || n=block_size);
              return buf)))
    in
    let write: BLK.r -> BLK.t -> unit m = (fun r buf -> 
        safely __LOC__ (
          ops.get_fd ()
          |> bind Unix.(fun fd ->
              ignore (lseek fd (r * block_size) SEEK_SET);
              let n = single_write fd buf 0 block_size in
              assert (n=block_size);
              return ())))
    in
    let disk_sync: unit -> unit m = (fun () -> 
        safely __LOC__ (
          ops.get_fd () |> bind (fun fd -> ExtUnixSpecific.fsync fd; return ())))
    in
    {block_size;read;write;disk_sync}
  )

end
