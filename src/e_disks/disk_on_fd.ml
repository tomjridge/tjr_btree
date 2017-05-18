(** Implement disk operations backed by a file-descriptor *)

open Prelude
open Btree_api
open Default
open Base_types.Monad

type fd = Unix.file_descr

open Mref
type 't fd_ops = (fd,'t) Mref.mref

(* don't want to clobber any other "safely" *)
let safely_ : string -> ('a,'t) m -> ('a,'t) m = (
  fun msg m ->
  fun s -> 
    try m s 
    with e -> (s,Error (msg ^ (Printexc.to_string e))))

let make_disk block_size ops = (
  let read: BLK.r -> (BLK.t,'t) m = (fun r ->
      safely_ __LOC__ (
        ops.get ()
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
      safely_ __LOC__ (
        ops.get ()
        |> bind Unix.(fun fd ->
            ignore (lseek fd (r * block_size) SEEK_SET);
            let buf = BLK.to_string buf in
            let n = single_write fd buf 0 block_size in
            assert (n=block_size);
            return ())))
  in
  (*
  let disk_sync: unit -> (unit,'t) m = (fun () -> 
      safely_ __LOC__ (
        ops.get () 
        |> bind (fun fd -> ExtUnixSpecific.fsync fd; return ())))
  in
*)
  {block_size;read;write (*;disk_sync *)}
)
