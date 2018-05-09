(** Leaf stream state. Needed early for store_ops *)

(* NOTE this type is made abstract in isa_export; it supports step,
   dest_LS_leaf, and lss_is_finished ops *)
type ('k,'v,'r) ls_state = ('k,'v,'r) Isa_export.Leaf_stream_state.ls_state
