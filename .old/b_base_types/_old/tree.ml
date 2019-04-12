(** The B-tree tree type. *)

open Isa_btree
include Isa_btree.Isa_export.Tree

(* pull this out for recap *)
module Tree_type = struct
  type ('a,'b) tree = ('a,'b) Isa_export.Tree.tree = Node of ('a list * ('a, 'b) tree list) |
    Leaf of ('a * 'b) list [@@deriving yojson]
end

(* save *)
let wellformed_tree' = wellformed_tree


let wellformed_tree ~constants ~maybe_small ~cmp =
  let constants = Constants.x_constants constants in
  let cmp x y = cmp x y |> Constants.Isabelle_conversions'.int_to_int in
  wellformed_tree' constants maybe_small cmp

let _ = wellformed_tree 
