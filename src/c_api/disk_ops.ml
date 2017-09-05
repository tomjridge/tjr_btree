(** Disk operations: read, write, and sync TODO. *)

(* FIXME move to int64 blkid *)

open Base_types
open Block

(* block device ---------------------------------------- *)

let wf_disk_ops 
    ~(blk_sz:blk_sz) 
    ~(read:blk_id -> (blk,'t) m) 
    ~(write:blk_id -> blk -> (unit,'t) m) 
  = true


let mk_disk_ops ~blk_sz ~read ~write =
  `Disk_ops(blk_sz,read,write)


let dest_disk_ops (`Disk_ops(blk_sz,read,write)) = 
  assert(wf_disk_ops ~blk_sz ~read ~write);
  fun k -> k ~blk_sz ~read ~write


let wf_imperative_disk_ops 
    ~(blk_sz:blk_sz)
    ~(read:blk_id -> blk)
    ~(write:blk_id -> blk -> unit)
  = true


let disk_ops_to_imperative ~blk_sz ~read ~write = 
  assert(wf_disk_ops ~blk_sz ~read ~write);
  fun ~_ref -> 
    let read r = 
      read r |> Monad.run !_ref |> fun (t',Ok blk) -> 
      _ref:=t';
      blk
    in
    let write r blk =
      write r blk |> Monad.run !_ref |> fun (t',Ok ()) ->
      _ref:=t';
      ()
    in
    assert(wf_imperative_disk_ops ~blk_sz ~read ~write);
    `Imperative_disk_ops(blk_sz,read,write)


(* old -------------------------------------------------------------- *)


(*
type 't disk_opsFIXME = {
  blk_sz: blk_sz;
  read: blk_id -> (blk,'t) m;
  write: blk_id -> blk -> (unit,'t) m;
(*   disk_sync: unit -> (unit,'t) m; *)
}
*)

(*
module Imperative_disk_ops = struct
  type idisk_ops = {
    blk_sz: blk_sz;
    read: blk_id -> blk;
    write: blk_id -> blk -> unit;
  }


  let of_disk_ops (ops:'t disk_ops) (_ref:'t ref) = (
    let read r = 
      ops.read r |> Monad.run !_ref |> fun (t',Ok blk) -> 
      _ref:=t';
      blk
    in
    let write r blk =
      ops.write r blk |> Monad.run !_ref |> fun (t',Ok ()) ->
      _ref:=t';
      ()
    in
    { blk_sz=ops.blk_sz; read; write}
  )
end
*)
