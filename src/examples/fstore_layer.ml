(** The fstore, which sits above the blk layer, but comes conceptually
   before it in order to allow btree_from_file to use fstore *)

open Btree_intf
(* open Tjr_profile.Util.Profiler *)
(* open Blk_layer *)


(* common to all impls *)
type blk_allocator_state = {
  min_free_blk_id:int;
}

(** {2 Block ops} *)

module Block_ops = struct
  let blk_sz = 4096

  type blk_id = int
  type blk = string

  let block_ops = String_block_ops.make_string_block_ops ~blk_sz 
end
include Block_ops


module Fstore = struct

  module Internal = struct
    (** A store, which we mutably change while initializing; only used
        for allocating refs; don't use otherwise *)
    let _fstore = ref Tjr_store.initial_store

    let alloc_fstore_ref = 
      fun x -> 
      Tjr_store.mk_ref x !_fstore |> fun (store',r) ->
      _fstore:=store';
      r

    let _ = alloc_fstore_ref
  end
  open Internal

  (* Convert state passing to imperative style using an ocaml
     ref. NOTE this takes the fstore argument as a ref. *)
  let run ~fstore (op:('a,fstore_passing)m) : 'a =
    State_passing.to_fun op (!fstore) |> fun (a,fstore') -> 
    fstore:=fstore';
    a

  
  (** We pull out the ref initialization, which is a bit scary and
     which we prefer to have all in one place *)

  (** The first block that can be allocated; currently 2, since 0 is
      the root block, and 1 is the initial empty leaf (for an empty
      B-tree; may be mutated) *)
  let first_free_block = 2
  let blk_allocator_ref = alloc_fstore_ref {min_free_blk_id=first_free_block} 

  (** This block stores the initial empty leaf *)
  let initial_btree_root_block : blk_id = 1
  let btree_root_block_ref = alloc_fstore_ref initial_btree_root_block

  (** Btree root ops. NOTE the type depends only on blk_id *)
  let root_ops = Fstore_passing.fstore_ref_to_with_state btree_root_block_ref
  let root_ops = {root_ops}

  let in_mem_blk_dev_ref = 
    let m : (blk_id,blk)Tjr_map.With_pervasives_compare.map_with_pervasives_compare = (Tjr_map.With_pervasives_compare.empty ()) in
    let r = alloc_fstore_ref m in
    r

  let on_disk_blk_dev_ref = alloc_fstore_ref (None:Unix.file_descr option)
end
open Fstore


module Monad_ops = struct
  let monad_ops = fstore_passing_monad_ops
  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return
end


module Internal = struct
  (** {2 Block allocator} *)

  let blk_allocator : (blk_allocator_state,fstore_passing) with_state = 
    Fstore_passing.fstore_ref_to_with_state blk_allocator_ref

end
