(** Help for maps on fd: write global state into root block; load back
   in from file *)

(* FIXME we already know what the empty disk leaf is, but the
   marshalling params allow us to convert to a blk; perhaps assume we
   already have empty_leaf_as_blk? *)

module type S = sig
  type blk
  val block_ops: blk block_ops

  type dnode
  val mp: (dnode,blk) marshalling_ops

  val empty_disk_leaf_as_blk: blk
end


module Make(S:S) = struct
  open S

  (** {2 Root blocks}

      We implement the map on fd by writing the free counter and root
      page_ref into the root block 

  *)

  (* TODO we use standard ocaml marshalling for the root block - this is
     a demo anyway *)
  let marshal_to_string (x:int*int) = Marshal.to_string x []

  let marshal_from_string s : int*int = Marshal.from_string s 0

  let root_blk_id = 0

  let write_root_block ~fd ~free ~root = 
    (free,root)
    |> marshal_to_string 
    |> block_ops.of_string |> fun blk -> 
    Blk_dev_on_fd.Internal.write ~block_ops ~fd ~blk_id:root_blk_id ~blk

  let _ : fd:Unix.file_descr -> free:int -> root:int -> unit
    = write_root_block

  let read_root_block ~fd = 
    Blk_dev_on_fd.Internal.read ~block_ops ~fd ~blk_id:root_blk_id 
    |> block_ops.to_string 
    |> (fun x -> (marshal_from_string x : (int * int)))
    |> fun (free,root) -> (free,root)

  (** NOTE empty_disk_leaf and mp only needed for init *)
  let from_file ~fn ~create ~init = 
    let fd = Tjr_file.fd_from_file ~fn ~create ~init in
    match init with
    | true -> (
        (* now need to write the initial dnode *)
        let _ = 
          let blk = empty_disk_leaf_as_blk in
          Blk_dev_on_fd.Internal.write ~block_ops ~fd ~blk_id:1 ~blk
        in
        (* 0,1 are taken so 2 is free; 1 is the root of the btree FIXME
           this needs to somehow match up with Examples.first_free_block
        *)
        let (free,root) = (2,1) in
        (* remember to write blk0 *)
        let _ = write_root_block ~fd ~free ~root in
        (fd,free,root))
    | false -> (
        let (free,root) = read_root_block ~fd in 
        (fd,free,root))

  let _ : 
fn:string -> create:bool -> init:bool -> Unix.file_descr * int * int
= from_file

  let close ~fd ~free ~root = 
    write_root_block ~fd ~free ~root;
    Unix.close fd

  (** {2 State passing type with fd, free and root fields} *)

  open Page_ref_int

  (* the global state *)
  type state = {
    fd: Unix.file_descr;
    free: page_ref;
    root: page_ref; (* pointer to root of btree *)
  }        
  type t = state


  let monad_ops : t state_passing monad_ops = State_passing.monad_ops ()

  (* FIXME change to with_state? 
     let fd_ops = {
     get=(fun () -> with_world (fun w -> (w.fd,w)));
     set=(fun fd -> with_world (fun w -> ((),{w with fd}))); 
     }

     let free_ops = {
     get=(fun () -> with_world (fun t -> (t.free,t)));
     set=(fun free -> with_world (fun t -> ((),{t with free})));
     }

     let _page_ref_ops = {
     get=(fun () -> with_world (fun t -> (t.root,t)));
     set=(fun root -> with_world (fun t -> ((),{t with root})));
     }

     let root_ops = _page_ref_ops
  *)

  module Export = struct
    let from_file ~fn ~create ~init = 
      from_file ~fn ~create ~init |> fun (fd,free,root) -> 
      {fd;free;root}

    let close t = close ~fd:t.fd ~free:t.free ~root:t.root
  end

end





