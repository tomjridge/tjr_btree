(** Disk block operations: read, write *)

(* TODO and sync *)

open Base_types
open Block

(* block device ---------------------------------------- *)

module Block_device_type = struct
  type 't block_device = {
    blk_sz:blk_sz;
    read:blk_id -> (blk,'t) m;
    write:blk_id -> blk -> (unit,'t) m
  }
end
include Block_device_type


let wf_disk_ops 
    ~(blk_sz:blk_sz) 
    ~(read:blk_id -> (blk,'t) m) 
    ~(write:blk_id -> blk -> (unit,'t) m) 
  = true[@@ocaml.warning "-8-27"]


let mk_disk_ops ~blk_sz ~read ~write =
  {blk_sz; read; write}  (* `Disk_ops(blk_sz,read,write) *)


let dest_disk_ops ops = 
  let blk_sz,read,write = ops.blk_sz,ops.read,ops.write in
  assert(wf_disk_ops ~blk_sz ~read ~write);
  fun k -> k ~blk_sz ~read ~write


let wf_imperative_disk_ops 
    ~(blk_sz:blk_sz)
    ~(read:blk_id -> blk)
    ~(write:blk_id -> blk -> unit)
  = true[@@ocaml.warning "-8-27"]


(* FIXME? leave imperative disk ops as a polymorphic variant for the
   time being *)
(* NOTE this assumes that the block device never halts; maybe we need
   to also supply a ~halt function, and use
   Tjr_step_monad.Extra.run_with_halt *)

(* FIXME this needs to be put somewhere else when we have particular monad instantiations *)
(*
let disk_ops_to_imperative ~blk_sz ~read ~write = 
  assert(wf_disk_ops ~blk_sz ~read ~write);
  fun ~_ref -> 
    let read r = 
      read r |> Tjr_step_monad.Extra.run !_ref |> fun (t',blk) -> 
      _ref:=t';
      blk
    in
    let write r blk =
      write r blk |> Tjr_step_monad.Extra.run !_ref |> fun (t',()) ->
      _ref:=t';
      ()
    in
    assert(wf_imperative_disk_ops ~blk_sz ~read ~write);
    `Imperative_disk_ops(blk_sz,read,write)


*)
