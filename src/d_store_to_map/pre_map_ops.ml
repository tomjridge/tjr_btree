(** A pre-map is like a map, but functions take an additional reference (to the
   root of the B-tree), and return (the updated state and) an updated reference.
*)

open Base_types
open Prelude

(*
type ('k,'v,'r,'t) pre_map_ops = { 
  (** NOTE the return value includes a reference to a leaf, not a
     reference to the updated B-tree... find does not alter the B-tree
     *)
  find_leaf: 'k -> 'r -> ('r*('k*'v)list*('k,'r)rstk,'t) m; 
  find: 'k -> 'r -> ('r * ('k*'v)list,'t) m; 
  insert: 'k -> 'v -> 'r -> ('r,'t) m;
  insert_many: 'k -> 'v -> ('k*'v)list -> 'r -> ('r * ('k*'v)list,'t) m; 
  delete: 'k -> 'r -> ('r,'t) m; 
}
*)

let wf_pre_map_ops   
    (** NOTE the return value includes a reference to a leaf, not a
        reference to the updated B-tree... find does not alter the B-tree
    *)
    ~(find_leaf: 'k -> 'r -> ('r*('k*'v)list*('k,'r)rstk,'t) m)
    ~(find: 'k -> 'r -> ('r * ('k*'v)list,'t) m)
    ~(insert: 'k -> 'v -> 'r -> ('r,'t) m)
    ~(insert_many: 'k -> 'v -> ('k*'v)list -> 'r -> ('r * ('k*'v)list,'t) m)
    ~(delete: 'k -> 'r -> ('r,'t) m)
  = 
  true

let mk_pre_map_ops ~find_leaf ~find ~insert ~insert_many ~delete =
  assert(wf_pre_map_ops ~find_leaf ~find ~insert ~insert_many ~delete);
  `Pre_map_ops(find_leaf,find,insert,insert_many,delete)

let dest_pre_map_ops (`Pre_map_ops(find_leaf,find,insert,insert_many,delete)) =
  fun k -> k ~find_leaf ~find ~insert ~insert_many ~delete
