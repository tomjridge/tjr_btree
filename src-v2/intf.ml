
type constants = {
  max_leaf_keys   : int;
  max_branch_keys : int;
}

type 'k k_cmp = {
  k_cmp: 'k -> 'k -> int
}

(** NOTE Leaves are mutable *)
type ('k,'v,'leaf) leaf_ops = {
  lookup           : 'k -> 'leaf -> 'v option;
  insert           : 'k -> 'v -> 'leaf -> unit;
  remove           : 'k -> 'leaf -> unit;
  leaf_nkeys       : 'leaf -> int;
  split_leaf       : int -> 'leaf -> 'leaf*'k*'leaf;  (* first leaf has n keys *)
  to_kvs           : 'leaf -> ('k*'v) list;
  of_kvs           : ('k*'v) list -> 'leaf;
}


module Top_or_bottom = struct
  type 'k or_top = 'k option

  type 'k or_bottom = 'k option

end
open Top_or_bottom

let k_lt ~k_cmp ~k1 ~k2 = 
  match k2 with 
  | None -> true
  | Some k2 -> k_cmp.k_cmp k1 k2 < 0

let k_leq ~k_cmp ~k1 ~k2 =
  match k1 with
  | None -> true
  | Some k1 -> k_cmp.k_cmp k1 k2 < 0

type ('k,'r) segment = 'k or_bottom * 'r * ('k*'r) list * 'k or_top

(** NOTE Nodes are mutable *)
type ('k,'r,'branch) branch_ops = {
  find             : 'k -> 'branch -> ('k or_bottom*'r*'k or_top);
  branch_nkeys     : 'branch -> int;
  split_branch     : int -> 'branch -> 'branch * 'k * 'branch; (* first branch has n keys; n>=1 *)
  make_small_root  : 'r*'k*'r -> 'branch;
  to_krs           : 'branch -> ('k list * 'r list);
  of_krs           : ('k list * 'r list) -> 'branch;
  replace          : ('k,'r) segment -> ('k,'r) segment -> 'branch -> unit;
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
