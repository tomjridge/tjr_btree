(** B-tree constants: minimum leaf size etc. These constants should be
   chosen so that nodes and leaves fit in a block. Clearly this
   depends on the block size, the size of keys and values, the
   on-disk format etc. *)
type t = {
  min_leaf_size: int;
  max_leaf_size: int;
  min_node_keys: int;
  max_node_keys: int
}

(** Construct constants given the block/page size, and the details of
   the on-disk format. Here we assume the strategy implemented in
   Btree_with_pickle, and so we must also provide details of the
   length of keys and values when stored on disk. *)
let make_constants ~page_size ~tag_len ~k_len ~v_len = (
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
