type t = {
  min_leaf_size: int;
  max_leaf_size: int;
  min_node_keys: int;
  max_node_keys: int
}

let make_constants page_size tag_len k_len v_len = (
  let max_leaf_size = 
    (page_size - tag_len - tag_len) (* for tag and length *)
    / (k_len+v_len)
  in
  let max_node_keys =
    (page_size - tag_len - tag_len - tag_len (* tag, length x 2 *)
     - v_len) (* always one val more than # keys *)
    / (k_len + v_len)
  in
  let min_leaf_size = 2 in
  let min_node_keys = 2 in
  { min_leaf_size; max_leaf_size; min_node_keys; max_node_keys}
)
