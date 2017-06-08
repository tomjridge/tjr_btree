(** Various common parameters to functions. *)

(** 

The code is heavily parameterized. We don't want function
arguments to be too numerous. So we typically have a single
"parameters" object ps which is more-or-less untyped. To ensure a
parameter is always accessed using a particular name, and to
enforce that the value of the parameter has a given type, we define
"parameter accessor" functions below.

*) 

open Frame
open Tree
open R2t

(*
let compare_k x : 'k -> 'k -> int = (x#compare_k)
*)

(** The order on keys. B-trees work with ordered keys. *)
let cmp x : 'k -> 'k -> int = (x#cmp)

(** Constants. See {!Constants} *)
let constants x : Constants.t = (x#constants)

(*
(** Store operations. See {!Store_ops} *)
let store_ops x : ('k,'v,'r,'t) store_ops = (x#store_ops)
*)

open Yojson.Safe

(** Debugging parameters. Includes r2t (see {!R2t}) and various
   conversions to json for keys, values and references. *)
(** Debugging parameters are usually optional. Use (dbg x) to get debug parameters. *)
let r2t x : ('k,'v,'r,'t) r2t = x#r2t

let k2j x : 'k -> json = x#k2j

let v2j x : 'v -> json = x#v2j

let r2j x : 'r -> json = x#r2j

let dbg_ps x = x#dbg_ps

(** Block size, i.e., the number of bytes that can be stored
   atomically on-disk in a single block. *)
let blk_sz x : int = (x#blk_sz)

(** Pages are blocks in memory. This parameter is a synonym for [block_size]. *)
let page_size = blk_sz

(*
open Pickle_params

let pp x : ('k,'v) pp = x#pp
*)

open Block
open Frame

let page_to_frame x : blk -> ('k,'v,'r) frame = x#page_to_frame

(* following takes block size to allow uniformity *)
let frame_to_page x : int -> ('k,'v,'r) frame -> blk = x#frame_to_page

(* these are typically part of "extended" parameters *)

let page_ref_ops x = x#page_ref_ops

let store_ops x = x#store_ops




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


