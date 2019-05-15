(** Export the main functionality of this library. *)

(** {2 Blk_allocator} *)

include Blk_allocator_ops_type


(** {2 Map_ops and extra_map_ops} *)

include Map_ops_type


(** {2 Marshalling } *)

include Marshalling_ops_type


(** {2 Disk_to_store} *)

include Disk_to_store


(** {2 Store_to_map (main B-tree functionality)} *)

include Store_to_map

