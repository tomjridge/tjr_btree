(** A simple example of a kv store; k=string, v=string. *)

open Generic_example
open Examples.Ss_ss

let int_to_k = fun x -> string_of_int x |> Small_string.of_string
let int_to_v = fun x -> string_of_int x |> Small_string.of_string

module A = (val make_generic_example ~btree_from_file ~map_ops_etc ~int_to_k ~int_to_v : T)
include A
