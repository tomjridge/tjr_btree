(** The fstore, which sits above the blk layer, but comes conceptually
   before it in order to allow btree_from_file to use fstore *)

open Btree_intf

(* common to all impls *)
type blk_allocator_state = {
  min_free_blk_id:int;
}

(** {2 Block ops} *)

module Blk_id = Blk_id_as_int
open Blk_id

(* open Tjr_fs_shared.Blk_intf. *)
type blk = string
let blk_sz = Blk_sz.blk_sz_4096 
let blk_ops = Common_blk_ops.String_.make ~blk_sz

(*
module Block_ops = struct
  type blk_id = int
  let blk_sz = 4096
    
  type blk = string
  let blk_ops = Common_blk_ops.String_.make ~blk_sz 
end
include Block_ops
*)

module Initial_values = struct

  (** The first block that can be allocated; currently 2, since 0 is
      the root block, and 1 is the initial empty leaf (for an empty
      B-tree; may be mutated) *)
  let first_free_block = 2

  (** This block stores the initial empty leaf *)
  let initial_btree_root_block = (Blk_id.of_int 1)
end

module Fstore = struct
  open Initial_values

  (* Convert state passing to imperative style using an ocaml
     ref. NOTE this takes the fstore argument as a ref. *)
  let run ~fstore (op:('a,fstore_passing)m) : 'a =
    State_passing.to_fun op (!fstore) |> fun (a,fstore') -> 
    fstore:=fstore';
    a

  (* NOTE: we allow reset for on_disk_blk_dev_ref *)
  module R = Tjr_store.Make_imperative_fstore(struct let allow_reset=true end)

  let blk_allocator_ref = R.ref {min_free_blk_id=first_free_block} 
  let blk_allocator : (blk_allocator_state,fstore_passing) with_state = 
    Fstore_passing.fstore_ref_to_with_state blk_allocator_ref

  let btree_root_block_ref = R.ref initial_btree_root_block

  let in_mem_blk_dev_ref = 
    let m : (blk_id,blk)Tjr_map.With_pervasives_compare.map_with_pervasives_compare = (Tjr_map.With_pervasives_compare.empty ()) in
    R.ref m

  let on_disk_blk_dev_ref = R.ref (None:Unix.file_descr option)


  (** Btree root ops. NOTE the type depends only on blk_id *)
  let root_ops = Fstore_passing.fstore_ref_to_with_state btree_root_block_ref
  let root_ops = {root_ops}
end


module Monad_ops = struct
  let monad_ops = fstore_passing_monad_ops
  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return
end
