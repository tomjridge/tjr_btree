(** A simple example of a kv store. *)

(* open Examples *)
open Generic_example

let int_to_k = fun x -> x
let int_to_v = fun x -> x

module A = (val make_generic_example ~btree_from_file:(failwith "") ~map_ops_etc:(failwith "") ~int_to_k ~int_to_v : T)
include A
