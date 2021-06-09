
(** NOTE Leaves are mutable *)
type ('k,'v,'leaf) leaf_ops = {
  lookup           : 'k -> 'leaf -> 'v option;
  insert           : 'k -> 'v -> 'leaf -> unit;
  remove           : 'k -> 'leaf -> unit;
  (* length           : 'leaf -> int; *)
  is_large         : 'leaf -> bool;
  split_large_leaf : 'leaf -> 'leaf*'k*'leaf;
  to_kvs           : 'leaf -> ('k*'v) list;
  of_kvs           : ('k*'v) list -> 'leaf;
}


module Top_or_bottom = struct
  type 'k or_top = 'k option

  type 'k or_bottom = 'k option
end
open Top_or_bottom

type ('k,'r) segment = 'k or_bottom * 'r * ('k*'r) list * 'k or_top

(** NOTE Nodes are mutable *)
type ('k,'r,'branch) branch_ops = {
  find             : 'k -> 'branch -> ('k or_bottom*'r*'k or_top);
  is_large         : 'branch -> bool;
  split_large_branch : 'branch -> 'branch * 'k * 'branch;
  make_small_root  : 'r*'k*'r -> 'branch;
  to_krs           : 'branch -> ('k list * 'r list);
  of_krs           : ('k list * 'r list) -> 'branch;
  replace          : ('k,'r) segment -> ('k,'r) segment -> 'branch -> unit;
  (* keys_length      : 'branch -> int; *)
}

type ('branch,'leaf,'node) node_ops = {
(*  typ : 'node -> [ `Branch | `Leaf ];
  dest_Branch : 'node -> 'branch;
  dest_Leaf : 'node -> 'leaf; *)
  cases: 'a. 'node -> leaf:('leaf -> 'a) -> branch:('branch -> 'a) -> 'a;
  of_leaf: 'leaf -> 'node;
  of_branch: 'branch -> 'node;
}


type ('r,'node,'t) store_ops = {
  read : 'r -> ('node,'t)m;
  write : 'r -> 'node -> (unit,'t)m;
}
