(** Various common parameters to functions. *)

(** 

The code is heavily parameterized. We don't want function
arguments to be too numerous. So we typically have a single
"parameters" record.

*) 

open Store_ops
open Frame
open Bt_tree
open R2t
module Constants = Bt_constants

open Yojson.Safe

(** Debugging parameters. Includes r2t (see {!R2t}) and various
   conversions to json for keys, values and references. Passed
   separately to normal parameters. Shortened to [dbg_ps] *)
type ('k,'v,'r,'t,'tree,'o) debug_params = {
  r2t: ('k,'v,'r,'t) r2t;
  tree2j: 'tree -> json;
  op_state2j: 'o -> json;
}


(*  k2j: 'k -> json;
  v2j: 'v -> json;
  r2j: 'r -> json *)

(*
type ('a,'b,'c,'d,'e) params = {
  (** The order on keys. B-trees work with ordered keys. *)
  compare_k: 'a option;
  (** Constants. See {!Constants} *)
  constants: Constants.t;
  (** Block size, i.e., the number of bytes that can be stored
      atomically on-disk in a single block. *)
  blk_sz : int;
}
*)

(* let store_ops x : ('k,'v,'r,'t) store_ops = (x#store_ops) *)



(** Pages are blocks in memory. This parameter is a synonym for [block_size]. *)
(* let page_size = blk_sz *)

