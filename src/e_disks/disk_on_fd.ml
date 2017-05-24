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

(* raw operations --------------------------------------------------- *)

let read fd block_size r = Unix.(
  ignore (lseek fd (r * block_size) SEEK_SET);
  let buf = Bytes.make block_size (Char.chr 0) in 
  let n = read fd buf 0 block_size in
  (* assert (n=block_size); we allow the file to expand
               automatically, so no reason to read any bytes *)
  assert(n=0 || n=block_size);
  BLK.of_string block_size buf)


let write ~fd ~block_size ~blk_id ~blk = Unix.(
    ignore (lseek fd (blk_id * block_size) SEEK_SET);
    let buf = BLK.to_string blk in
    let n = single_write fd buf 0 block_size in
    assert (n=block_size);
    ())


(* in the monad ----------------------------------------------------- *)

let make_disk block_size ops = (
  let read: BLK.r -> (BLK.t,'t) m = (fun r ->
      safely_ __LOC__ (
        ops.get ()
        |> bind (fun fd ->
            return (read fd block_size r))))
  in
  let write: BLK.r -> BLK.t -> (unit,'t) m = (fun r buf -> 
      safely_ __LOC__ (
        ops.get ()
        |> bind (fun fd ->           
            return (write fd block_size r buf))))
  in
  {block_size;read;write})

