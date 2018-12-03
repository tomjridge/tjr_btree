type ('a, 'b, 'c) frame =
  ('a, 'b, 'c) Isa_export.Frame.frame =
    Node_frame of ('a list * 'c list)
  | Leaf_frame of ('a * 'b) list
val dest_Leaf_frame : ('a, 'b, 'c) frame -> ('a * 'b) list
val dest_Node_frame : ('a, 'b, 'c) frame -> 'a list * 'c list
