(* Disk on a file-descriptor *)

open Prelude
open Btree_api

type fd = Unix.file_descr

type 't fd_ops = {
  get_fd: unit -> (fd,'t) m;
  set_fd: fd -> (unit,'t) m;
}

let safely : string -> ('a,'t) m -> ('a,'t) m = (
  fun msg m ->
  fun s -> 
    try m s 
    with e -> (s,Error (msg ^ (Printexc.to_string e))))

open Simple_monad

let make_disk block_size ops = (
  let read: BLK.r -> (BLK.t,'t) m = (fun r ->
      safely __LOC__ (
        ops.get_fd ()
        |> bind Unix.(fun fd ->
            ignore (lseek fd (r * block_size) SEEK_SET);
            let buf = Bytes.make block_size (Char.chr 0) in 
            let n = read fd buf 0 block_size in
            (* assert (n=block_size); we allow the file to expand
               automatically, so no reason to read any bytes *)
            assert(n=0 || n=block_size);
            return (BLK.of_string block_size buf))))
  in
  let write: BLK.r -> BLK.t -> (unit,'t) m = (fun r buf -> 
      safely __LOC__ (
        ops.get_fd ()
        |> bind Unix.(fun fd ->
            ignore (lseek fd (r * block_size) SEEK_SET);
            let buf = BLK.to_string buf in
            let n = single_write fd buf 0 block_size in
            assert (n=block_size);
            return ())))
  in
  let disk_sync: unit -> (unit,'t) m = (fun () -> 
      safely __LOC__ (
        ops.get_fd () 
        |> bind (fun fd -> ExtUnixSpecific.fsync fd; return ())))
  in
  {block_size;read;write;disk_sync}
)
