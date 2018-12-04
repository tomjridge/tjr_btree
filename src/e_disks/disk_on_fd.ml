(** Implement disk operations backed by a file-descriptor


FIXME this depends on Unix, so maybe move elsewhere
 *)

open Base_types
open Block
open Test
(* open Disk_ops *)


type fd = Unix.file_descr


(** [fd_ops] identifies a filedescriptor in the global state *)
(* FIXME why use mref? why not have all operations parameterized by
   fd? because we want a uniform interface to disks, and in general they
   are not identified by fd 

FIXME move elsewhere?
*)
type 't fd_ops = (fd,'t) mref


(* raw operations --------------------------------------------------- *)

let read ~fd ~blk_sz ~blk_id = 
  ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
  let buf = Bytes.make blk_sz (Char.chr 0) in 
  let n = Unix.read fd buf 0 blk_sz in
  (* assert (n=blk_sz); we allow the file to expand automatically, so
     no reason to read any bytes since file could be empty *)
  test(fun _ -> assert(n=0 || n=blk_sz));
  Block.of_bytes blk_sz buf


let write ~fd ~blk_sz ~blk_id ~blk = 
  ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
  let buf = Block.to_bytes blk in
  let n = Unix.single_write fd buf 0 blk_sz in
  test(fun _ -> assert (n=blk_sz));
  ()


(* in the monad ----------------------------------------------------- *)

(** Construct [disk_ops] *)
let make_disk ~monad_ops ~blk_sz ~fd_ops = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let read: blk_id -> (blk,'t) m = fun r ->
    fd_ops.get () >>= fun fd ->
    return (read ~fd ~blk_sz ~blk_id:r)
  in
  let write: blk_id -> blk -> (unit,'t) m = fun r buf -> 
    fd_ops.get () >>= fun fd ->           
    return (write ~fd ~blk_sz ~blk_id:r ~blk:buf)
  in
  Disk_ops.mk_disk_ops ~blk_sz ~read ~write
