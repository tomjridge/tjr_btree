(** A frame is a node in a B-tree. 'k is the key type. 'v is the value
   type. 'r is the type of pointers to blocks. *)
include Isa_btree.Isa_export.Disk_node


module Frame_type = struct
type ('k,'v,'r) frame = ('k,'v,'r) Isa_btree.Isa_export.Disk_node.dnode = Disk_node of ('k list * 'r list) |
    Disk_leaf of ('k * 'v) list  [@@deriving yojson]
end
include Frame_type
