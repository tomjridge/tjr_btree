(** A frame is a node in a B-tree. 'k is the key type. 'v is the value
   type. 'r is the type of pointers to blocks. *)
include Isa_export.Disk_node


module Frame_type = struct
type ('k,'v,'r) frame = ('k,'v,'r) Isa_export.Disk_node.dnode
end
include Frame_type
