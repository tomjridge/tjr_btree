type ('a, 'b) tree =
  ('a, 'b) Isa_export.Tree.tree =
    Node of ('a list * ('a, 'b) tree list)
  | Leaf of ('a * 'b) list
val height : ('a, 'b) tree -> Isa_export.Arith.nat
val dest_Node : ('a, 'b) tree -> 'a list * ('a, 'b) tree list
val tree_equal : ('a, 'b) tree -> ('a, 'b) tree -> bool
val tree_to_leaves : ('a, 'b) tree -> ('a * 'b) list list
val tree_to_kvs : ('a, 'b) tree -> ('a * 'b) list
val tree_to_keys : ('a, 'b) tree -> 'a Isa_export.Set.set
val wellformed_tree :
  unit Isa_export.Prelude.constants_ext ->
  Isa_export.Prelude.min_size_t option ->
  ('a -> 'a -> Isa_export.Arith.int) -> ('a, 'b) tree -> bool
