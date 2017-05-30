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

let read ~fd ~blk_sz ~blk_id = Unix.(
  ignore (lseek fd (blk_id * blk_sz) SEEK_SET);
  let buf = Bytes.make blk_sz (Char.chr 0) in 
  let n = read fd buf 0 blk_sz in
  (* assert (n=blk_sz); we allow the file to expand
               automatically, so no reason to read any bytes *)
  assert(n=0 || n=blk_sz);
  BLK.of_string blk_sz buf)


let write ~fd ~blk_sz ~blk_id ~blk = Unix.(
    ignore (lseek fd (blk_id * blk_sz) SEEK_SET);
    let buf = BLK.to_string blk in
    let n = single_write fd buf 0 blk_sz in
    assert (n=blk_sz);
    ())


(* in the monad ----------------------------------------------------- *)

let make_disk blk_sz fd_ops = (
  let read: BLK.r -> (BLK.t,'t) m = (fun r ->
      safely_ __LOC__ (
        fd_ops.get ()
        |> bind (fun fd ->
            return (read fd blk_sz r))))
  in
  let write: BLK.r -> BLK.t -> (unit,'t) m = (fun r buf -> 
      safely_ __LOC__ (
        fd_ops.get ()
        |> bind (fun fd ->           
            return (write fd blk_sz r buf))))
  in
  {blk_sz;read;write})

