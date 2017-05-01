(* a version with explicit page_ref passing *)

open Prelude

type ('k,'v,'r,'t) pre_map_ops = {
  find: 'k -> 'r -> ('r * ('k*'v)list,'t) m;
  insert: 'k -> 'v -> 'r -> ('r,'t) m;
  insert_many: 'k -> 'v -> ('k*'v)list -> 'r -> ('r * ('k*'v)list,'t) m;
  delete: 'k -> 'r -> ('r,'t) m;
}
