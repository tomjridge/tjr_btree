(** A pre-map is like a map, but functions take an additional reference (to the
   root of the B-tree), and return (the updated state and) an updated reference.
*)

open Base_types
open Prelude

type ('k,'v,'r,'t) pre_map_ops = { 
  (** NOTE the return value includes a reference to a leaf, not a
     reference to the updated B-tree... find does not alter the B-tree
     *)
  find: 'k -> 'r -> ('r * ('k*'v)list,'t) m; 
  insert: 'k -> 'v -> 'r -> ('r,'t) m;
  insert_many: 'k -> 'v -> ('k*'v)list -> 'r -> ('r * ('k*'v)list,'t) m; 
  delete: 'k -> 'r -> ('r,'t) m; 
}
