type t = {
  min_leaf_size : int;
  max_leaf_size : int;
  min_node_keys : int;
  max_node_keys : int;
}
val make_constants : int -> int -> int -> int -> t
