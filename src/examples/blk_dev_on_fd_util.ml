
(** Help for maps on fd: write global state into root block; load back
   in from file *)

(* FIXME we already know what the empty disk leaf is, but the
   marshalling params allow us to convert to a blk; perhaps assume we
   already have empty_leaf_as_blk? *)

(*
    let empty_disk_leaf_as_blk = 
      let blk = lazy (
        leaf_ops.kvs_to_leaf [] |> fun x ->
        mp.dnode_to_blk (Disk_leaf x))
      in
      fun () -> Lazy.force blk
*)

module Types = struct
  type blk_id = int

  (** FIXME if btree_root was an option, we could avoid passing empty_disk_leaf_as_blk *)
  type root_block = {
    free:blk_id;
    btree_root:blk_id
  }
end
include Types

type from_file_close = {
  from_file:fn:string -> create:bool -> init:bool -> Unix.file_descr * root_block;
  close: fd:Unix.file_descr -> root_block:root_block -> unit
}

(** Constructs [from_file] and [close] *)
let make (type blk) ~(block_ops:blk block_ops) ~(empty_disk_leaf_as_blk:unit -> blk) = 
  let module A = struct

    (** {2 Root blocks}

        We implement the map on fd by writing the free counter and root
        page_ref into the root block 

    *)

    (* TODO we use standard ocaml marshalling for the root block - this is
       a demo anyway *)
    let marshal_to_string (x:root_block) = Marshal.to_string x []

    let marshal_from_string s : root_block = Marshal.from_string s 0

    let root_blk_id = 0

    let write_root_block ~fd ~root_block = 
      root_block
      |> marshal_to_string 
      |> block_ops.of_string |> fun blk -> 
      Blk_dev_on_fd.Internal.write ~block_ops ~fd ~blk_id:root_blk_id ~blk

    let read_root_block ~fd = 
      Blk_dev_on_fd.Internal.read ~block_ops ~fd ~blk_id:root_blk_id 
      |> block_ops.to_string 
      |> (fun x -> (marshal_from_string x))

    let _ = read_root_block

    (** NOTE empty_disk_leaf only needed for init *)
    let from_file ~fn ~create ~init = 
      let fd = Tjr_file.fd_from_file ~fn ~create ~init in
      match init with
      | true -> (
          (* now need to write the initial dnode *)
          let _ = 
            let blk = empty_disk_leaf_as_blk () in
            Blk_dev_on_fd.Internal.write ~block_ops ~fd ~blk_id:1 ~blk
          in
          (* 0,1 are taken so 2 is free; 1 is the root of the btree FIXME
             this needs to somehow match up with Examples.first_free_block
          *)
          let root_block = {free=2; btree_root=1} in
          (* remember to write blk0 *)
          let _ = write_root_block ~fd ~root_block in
          (fd,root_block))
      | false -> (
          let rb = read_root_block ~fd in 
          (fd,rb))

    let close ~fd ~root_block = 
      write_root_block ~fd ~root_block;
      Unix.close fd

  end
  in
  A.{ from_file; close }

(** Prettier type: {%html:<pre>
block_ops:'a block_ops ->
empty_disk_leaf_as_blk:(unit -> 'a) ->
from_file_close
</pre>%} *)

let _ = make

