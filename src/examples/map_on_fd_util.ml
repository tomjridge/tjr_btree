(** Help for maps on fd: write global state into root block; load back
   in from file *)

open Tjr_fs_shared
open Block_ops
(* open Isa_btree.Isa_export_wrapper *)
open Tjr_btree

(* root block ------------------------------------------------------- *)

(** {2 Root blocks}

We implement the map on fd by writing the free counter and root
   page_ref into the root block 

*)

(* TODO we use standard ocaml marshalling for the root block - this is
   a demo anyway *)
let marshal_to_string x = Marshal.to_string x []

let marshal_from_string s = Marshal.from_string s 0

let root_blk_id = 0

let write_root_block ~block_ops ~fd ~free ~root = 
  (free,root) 
  |> marshal_to_string 
  |> block_ops.of_string |> fun blk -> 
  Blk_dev_on_fd.Internal.write ~block_ops ~fd ~blk_id:root_blk_id ~blk

let read_root_block ~block_ops ~fd = 
  Blk_dev_on_fd.Internal.read ~block_ops ~fd ~blk_id:root_blk_id 
  |> block_ops.to_string 
  |> (fun x -> (marshal_from_string x : (int * int)))
  |> fun (free,root) -> (free,root)

(** NOTE empty_disk_leaf only needed for init *)
let from_file ~block_ops ~mp ~fn ~create ~init ~empty_disk_leaf = 
  let fd = Tjr_file.fd_from_file ~fn ~create ~init in
  match init with
  | true -> (
      (* now need to write the initial dnode *)
      let _ = 
        let dn = empty_disk_leaf in
        let p = dn|>mp.dnode_to_blk in
        Blk_dev_on_fd.Internal.write ~block_ops ~fd ~blk_id:1 ~blk:p 
      in
      (* 0,1 are taken so 2 is free; 1 is the root of the btree FIXME
         this needs to somehow match up with Examples.first_free_block
         *)
      let (free,root) = (2,1) in
      (* remember to write blk0 *)
      let _ = write_root_block ~fd ~block_ops ~free ~root in
      (fd,free,root))
  | false -> (
      let (free,root) = read_root_block ~fd ~block_ops in 
      (fd,free,root))

let close ~block_ops ~fd ~free ~root = 
  write_root_block ~fd ~block_ops ~free ~root;
  Unix.close fd





(** {2 State passing type with fd, free and root fields} *)

open Page_ref_int
open Tjr_monad.Types
open Tjr_monad.State_passing
open Tjr_monad.Mref

(* the global state *)
type state = {
  fd: Unix.file_descr;
  free: page_ref;
  root: page_ref; (* pointer to root of btree *)
}        
type t = state


let monad_ops : t state_passing monad_ops = Tjr_monad.State_passing.monad_ops ()


let fd_ops = {
  get=(fun () -> with_world (fun w -> (w.fd,w)));
  set=(fun fd -> with_world (fun w -> ((),{w with fd}))); 
}

let free_ops = {
  get=(fun () -> with_world (fun t -> (t.free,t)));
  set=(fun free -> with_world (fun t -> ((),{t with free})));
}

let page_ref_ops = {
  get=(fun () -> with_world (fun t -> (t.root,t)));
  set=(fun root -> with_world (fun t -> ((),{t with root})));
}

let from_file ~block_ops ~mp ~fn ~create ~init ~empty_disk_leaf = 
  from_file ~block_ops ~mp ~fn ~create ~init ~empty_disk_leaf |> (fun (fd,free,root) -> 
      {fd;free;root})

let close ~block_ops t = 
  close ~block_ops ~fd:t.fd ~free:t.free ~root:t.root



