(** A frame is a node in a B-tree. 'k is the key type. 'v is the value
   type. 'r is the type of pointers to blocks. *)
include Isa_export.Disk_node

type ('k,'v,'r) frame = ('k,'v,'r) dnode

