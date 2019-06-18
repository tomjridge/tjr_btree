open Btree_intf
open Tjr_profile.Util.Profiler

(* common to all impls *)
type blk_allocator_state = {
  min_free_blk_id:int;
}

let blk_sz = 4096


(** A store, which we mutably change while initializing *)
let fstore = ref Tjr_store.initial_store

let alloc_fstore_ref = 
  fun x -> 
  Tjr_store.mk_ref x !fstore |> fun (store',r) ->
  fstore:=store';
  r

let _ = alloc_fstore_ref

type blk_id = int
type blk = string


module Monad_ops = struct
  let monad_ops = fstore_passing_monad_ops
  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return
end


module Internal = struct
  (** {2 Block ops} *)

  let block_ops = String_block_ops.make_string_block_ops ~blk_sz 

  (** {2 Block allocator} *)

  (** The first block that can be allocated; currently 2, since 0 is
      the root block, and 1 is the initial empty leaf (for an empty
      B-tree; may be mutated) *)
  let first_free_block = 2
  let blk_allocator_ref = alloc_fstore_ref {min_free_blk_id=first_free_block} 
  let blk_allocator : (blk_allocator_state,fstore_passing) with_state = 
    Fstore_passing.fstore_ref_to_with_state blk_allocator_ref


  (** {2 Btree root ops} *)

  let initial_btree_root_block : blk_id = 1
  let btree_root_block_ref = alloc_fstore_ref initial_btree_root_block
  let root_ops = Fstore_passing.fstore_ref_to_with_state btree_root_block_ref
  let root_ops = {root_ops}

end

let example_disk_ops ~blk_dev_ops ~reader_writers = 
  let open Monad_ops in
  (* block_ops and blk_allocator are reasonably free: the code doesn't
     depend on the exact details *)
  let block_ops,blk_allocator = Internal.(block_ops,blk_allocator) in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let make_disk_ops 
      ~(node_leaf_list_conversions:('k,'v,blk_id,'node,'leaf)Node_leaf_list_conversions.node_leaf_list_conversions)
    =
    let mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops
        ~node_leaf_list_conversions
        ~reader_writers
    in
    let mp = Btree_intf.{
        dnode_to_blk=(fun dn -> profile "d2blk" @@ fun () -> mp.dnode_to_blk dn);
        blk_to_dnode=(fun blk -> profile "blk2d" @@ fun () -> mp.blk_to_dnode blk);
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

let _ = example_disk_ops


module type BLK_DEV_OPS = sig
  (* open Internal *)
  val blk_dev_ops: (blk_id,blk,fstore_passing) blk_dev_ops
end


(** {2 In-memory block dev} *)

module In_mem_blk_dev : BLK_DEV_OPS = struct
  open Monad_ops
  let blk_dev_ops = 
    let blk_dev_ref = alloc_fstore_ref (Tjr_map.With_pervasives_compare.empty ()) in
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
  open Internal
  let blk_dev_ref = alloc_fstore_ref (None:Unix.file_descr option)

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
