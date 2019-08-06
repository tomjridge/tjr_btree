(** Main types for Tjr_btree_examples. NOTE that this sets blk_id to (a type iso to) int *)

(** {2 Binprot marshalling types} *)

(** These are in module {!Bin_prot_marshalling}. *)

(** {2 Blk_layer types} *)
module Blk_id = Blk_id_as_int
include Blk_id

(** A type to record a pair: the min free blk_id and the btree root block.

FIXME if btree_root was an option, we could avoid passing
   empty_disk_leaf_as_blk *)
type root_block = {
  free       :blk_id;
  btree_root :blk_id
}

(** FIXME why use fstore and fstore_passing? *)
type btree_from_file_result = {
  fd         :Unix.file_descr;
  root_block :root_block;  (* NOTE this is the initial root block *)
  fstore     :Tjr_store.fstore ref;
  run        :'a. ('a, fstore_passing) m -> 'a; (* uses the fstore field! *)
  close      :unit -> unit;  (* FIXME should be monadic ? *)
}

type btree_from_file = {
  btree_from_file: 
    fn:string -> create:bool -> init:bool -> btree_from_file_result
}

