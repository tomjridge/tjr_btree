
(** Help for maps on fd: write global state into root block; load back
   in from file *)

(* FIXME we already know what the empty disk leaf is, but the
   marshalling params allow us to convert to a blk; perhaps assume we
   already have empty_leaf_as_blk? *)

include Fstore_layer.Block_ops

module Types = struct

  (** FIXME if btree_root was an option, we could avoid passing empty_disk_leaf_as_blk *)
  type root_block = {
    free:blk_id;
    btree_root:blk_id
  }


  type btree_from_file_result = {
    fd:Unix.file_descr;
    root_block:root_block;  (* NOTE this is the initial root block *)
    fstore:Tjr_store.fstore ref;
    run:'a. ('a, fstore_passing) m -> 'a; (* uses the fstore field! *)
    close: unit -> unit;  (* FIXME should be monadic ? *)
  }

  type btree_from_file = {
    btree_from_file: 
      fn:string -> create:bool -> init:bool -> btree_from_file_result
  }
end
include Types

(*
type from_file_close = {
  from_file:fn:string -> create:bool -> init:bool -> Unix.file_descr * root_block;
  close: fd:Unix.file_descr -> root_block:root_block -> unit
}
*)

(** Constructs a btree from a file functionality. *)
let make_btree_from_file (type blk) ~(block_ops:blk block_ops) ~(empty_leaf_as_blk:blk) = 
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
            let blk = empty_leaf_as_blk in
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

    (** {2 Root block initialization/finalization... Init and close} *)
    open Fstore_layer.Fstore

    (** Given various parameters, this sets up the initial tjr_store,
       so that the various refs agree with the parameters. *)
    let init_tjr_store ~fd ~(root_block:root_block) =
      (* Printf.printf "Init with free=%d and root=%d\n%!" root_block.free root_block.btree_root; *)
      let set = Tjr_store.set in
      !Fstore_layer.Fstore.R.fstore
      |> set blk_allocator_ref {min_free_blk_id=root_block.free}
      |> set btree_root_block_ref root_block.btree_root
      |> set on_disk_blk_dev_ref (Some fd)

    let _ = init_tjr_store

    let close ~fd ~fstore =
      let free = (Tjr_store.get blk_allocator_ref !fstore).min_free_blk_id in
      let btree_root = Tjr_store.get btree_root_block_ref !fstore in
      (* Printf.printf "Close with free=%d and root=%d\n%!" free btree_root; *)
      close ~fd ~root_block:{free; btree_root}

    let btree_from_file ~fn ~create ~init =
      let fd,root_block = from_file ~fn ~create ~init in
      let fstore = ref (init_tjr_store ~fd ~root_block) in
      let run x = run ~fstore x in
      let close () = close ~fd ~fstore in
      { fd; root_block; fstore; run; close }

    let _ = btree_from_file

  end
  in
  { btree_from_file=A.btree_from_file }

let make_btree_from_file ~empty_leaf_as_blk = make_btree_from_file ~block_ops ~empty_leaf_as_blk

(** Prettier type: {%html:<pre>
empty_leaf_as_blk:blk -> btree_from_file
</pre>%} *)

let _ = make_btree_from_file




open Btree_intf
open Fstore_layer

module Blk_profiler = Make_profiler()
open Blk_profiler

let mark' s f = 
  mark s;
  f () |> fun r ->
  mark (s^"'");
  r

let make_disk_ops ~blk_dev_ops ~reader_writers = 
  let open Monad_ops in
  (* block_ops and blk_allocator are reasonably free: the code doesn't
     depend on the exact details *)
  let block_ops,blk_allocator = (block_ops,Fstore.blk_allocator) in
  (* let ( >>= ) = monad_ops.bind in *)
  (* let return = monad_ops.return in *)
  let make_disk_ops 
        ~(node_leaf_list_conversions:('k,'v,blk_id,'node,'leaf)Node_leaf_list_conversions.node_leaf_list_conversions)
    =
    let mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops
        ~node_leaf_list_conversions
        ~reader_writers
    in
    let mp = Btree_intf.{
        dnode_to_blk=(fun dn -> mark' "d2blk" (fun () -> mp.dnode_to_blk dn));
        blk_to_dnode=(fun blk -> mark' "blk2d" (fun () -> mp.blk_to_dnode blk));
        marshal_blk_size=mp.marshal_blk_size;
      }
    in
    let blk_allocator_ops = 
      let { with_state } = blk_allocator in
      let alloc () = with_state (fun ~state:s ~set_state ->
          set_state {min_free_blk_id=s.min_free_blk_id+1} >>= fun _ 
          -> return s.min_free_blk_id)
      in
      let free _blk_id = return () in
      {alloc;free}
    in
    let disk_ops = { marshalling_ops=mp; blk_dev_ops; blk_allocator_ops } in
    disk_ops
  in
  make_disk_ops

let _ = make_disk_ops
  

module type BLK_DEV_OPS = sig
  (* open Internal *)
  val blk_dev_ops: (blk_id,blk,fstore_passing) blk_dev_ops
end


(** {2 In-memory block dev} *)

module In_mem_blk_dev : BLK_DEV_OPS = struct
  open Monad_ops
  let blk_dev_ref = Fstore.in_mem_blk_dev_ref
  let blk_dev_ops = 
    let with_state = Tjr_fs_shared.Fstore_passing.fstore_ref_to_with_state blk_dev_ref in
    let _ = with_state in
    Blk_dev_in_mem.make 
      ~monad_ops 
      ~blk_sz:(bsz_of_int blk_sz)  (* FIXME why are we making this a separate type? *)
      ~with_state

  let _ = blk_dev_ops
end
let in_mem_blk_dev = In_mem_blk_dev.blk_dev_ops



(** {2 On-disk block dev} *)

module On_disk_blk_dev (* : BLK_DEV_OPS *) = struct
  open Monad_ops
  (* open Internal *)
  let blk_dev_ref = Fstore.on_disk_blk_dev_ref

  let with_state = Fstore_passing.fstore_ref_to_with_state blk_dev_ref

  (* reuse the internal functionality *)
  open Blk_dev_on_fd.Internal

  let read_count = Global.register ~name:"Examples.read_count" (ref 0)
  let write_count = Global.register ~name:"Examples.write_count" (ref 0)
 
  (* open Tjr_profile.Util.No_profiler *)

  let read ~blk_id = with_state.with_state (fun ~state:(Some fd) ~set_state:_ -> 
      profile "fb" @@ fun () -> 
      incr(read_count);
      read ~block_ops ~fd ~blk_id |> return) [@@warning "-8"]

  let write ~blk_id ~blk = with_state.with_state (fun ~state:(Some fd) ~set_state:_ -> 
      profile "fc" @@ fun () -> 
      incr(write_count);
      write ~block_ops ~fd ~blk_id ~blk |> return) [@@warning "-8"]
 
  let blk_dev_ops = { blk_sz=(bsz_of_int blk_sz); read; write }
end
let on_disk_blk_dev = On_disk_blk_dev.blk_dev_ops


(** Use the on-disk blk dev to construct various instances *)

module Int_int = struct
  let blk_dev_ops = on_disk_blk_dev

  let reader_writers = Bin_prot_marshalling.Reader_writers.int_int

  let disk_ops ~node_leaf_list_conversions = make_disk_ops ~blk_dev_ops ~reader_writers ~node_leaf_list_conversions
end

module Ss_ss = struct
  let blk_dev_ops = on_disk_blk_dev

  let reader_writers = Bin_prot_marshalling.Reader_writers.ss_ss

  let disk_ops ~node_leaf_list_conversions = make_disk_ops ~blk_dev_ops ~reader_writers ~node_leaf_list_conversions
end


module Ss_int = struct
  let blk_dev_ops = on_disk_blk_dev

  let reader_writers = Bin_prot_marshalling.Reader_writers.ss_int

  let disk_ops ~node_leaf_list_conversions = make_disk_ops ~blk_dev_ops ~reader_writers ~node_leaf_list_conversions
end
