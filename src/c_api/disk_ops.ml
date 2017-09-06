(** Disk block operations: read, write *)

(* TODO and sync *)

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


