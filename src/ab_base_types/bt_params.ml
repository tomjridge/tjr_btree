(** Various common parameters to functions. *)

(** The code is heavily parameterized. We don't want function
   arguments to be too numerous. So we typically have a single
   "parameters" object ps which is more-or-less untyped. To ensure a
   parameter is always accessed using a particular name, and to
   enforce that the value of the parameter has a given type, we define
   "parameter accessor" functions below.
*) 

open Store_ops
open Frame
open Bt_tree
open R2t
module Constants = Bt_constants

(** The order on keys. B-trees work with ordered keys. *)
let compare_k x : 'k -> 'k -> int = (x#compare_k)

(** Constants. See {!Constants} *)
let constants x : Constants.t = (x#constants)

(** Store operations. See {!Store_ops} *)
let store_ops x : ('k,'v,'r,'t) store_ops = (x#store_ops)

open Yojson.Safe

(** Debugging parameters. Includes r2t (see {!R2t}) and various
   conversions to json for keys, values and references. *)
type ('k,'v,'r,'t,'k2j,'v2j) debug = {
  r2t: ('k,'v,'r,'t) r2t;
  k2j: 'k -> json;
  v2j: 'v -> json;
  r2j: 'r -> json
}

(** Debugging parameters are usually optional. *)
let debug x : ('k,'v,'r,'t,'k2j,'v2j) debug option = (x#debug)

(** Block size, i.e., the number of bytes that can be stored
   atomically on-disk in a single block. *)
let block_size x : int = (x#block_size)

(** Pages are blocks in memory. This parameter is a synonym for [block_size]. *)
let page_size = block_size


(*
type 'k ps0 = { 
  compare_k: 'k -> 'k -> int; 
  constants: Constants.t 
}

(* TODO make these match up with store_ops *)
type ('k,'v,'r,'t) ps1 = { 
  ps0: 'k ps0;
  store_ops: ('k,'v,'r,'t) store_ops;
}
*)

(*let spec_tree x : ('k,'v) tree option = (x#spec_tree)*)

(* let t_state x : 't = (x#t_state) *)

(*
other common parameters:

r2t tree state 

*)
