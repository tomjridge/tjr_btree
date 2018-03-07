module Constants_type = struct

  (** B-tree constants: minimum leaf size etc. These constants should be
      chosen so that nodes and leaves fit in a block. Clearly this
      depends on the block size, the size of keys and values, the
      on-disk format etc. *)
  type t = {
    min_leaf_size: int;
    max_leaf_size: int;
    min_node_keys: int;
    max_node_keys: int
  }  [@@deriving yojson]

end
include Constants_type

let cs2s c = c |> to_yojson |> Yojson.Safe.pretty_to_string

(* l'>=2l-1; m' >= 2m *)

(* some typical examples *)

let mk_cs l l' m m' = {
  min_leaf_size=l;
  max_leaf_size=l';
  min_node_keys=m;
  max_node_keys=m'
}

let cs1112 = mk_cs 1 1 1 2
let cs1124 = mk_cs 1 1 2 4
let cs1136 = mk_cs 1 1 3 6

let cs2312 = mk_cs 2 3 1 2
let cs2324 = mk_cs 2 3 2 4
let cs2336 = mk_cs 2 3 3 5


let cs3512 = mk_cs 3 5 1 2
let cs3524 = mk_cs 3 5 2 4
let cs3536 = mk_cs 3 5 3 6




(** Construct constants given the block/page size, and the details of
   the on-disk format. Here we assume the strategy implemented in
   Btree_with_pickle, and so we must also provide details of the
   length of keys and values when stored on disk. *)
(*
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
*)


module Isabelle_conversions' = struct
  open Isa_export

  let int_to_nat x = Isa_export.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)
  let int_to_int x = Isa_export.(
      x |>Big_int.big_int_of_int|>(fun x -> Arith.Int_of_integer x))

  let x_cmp cmp x y = cmp x y |> int_to_int
  (*  let x_ps0 ~constants ~cmp : 'k IE.Params.ps0 = (
    Ps0(constants|>x_constants, cmp|>x_cmp)) *)

  let x_constants cs0 : unit Prelude.constants_ext = Prelude.Constants_ext(
      cs0.min_leaf_size|>int_to_nat,
      cs0.max_leaf_size|>int_to_nat,
      cs0.min_node_keys|>int_to_nat,
      cs0.max_node_keys|>int_to_nat,
      ())
end
include Isabelle_conversions'
