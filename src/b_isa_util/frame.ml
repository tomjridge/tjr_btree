(** A frame is a node in a B-tree. 'k is the key type. 'v is the value
   type. 'r is the type of pointers to blocks. *)
type ('k, 'v, 'r) frame = ('k,'v,'r) Isa_export.Frame.frame = 
    Node_frame of ('k list * 'r list) |
    Leaf_frame of ('k * 'v) list
