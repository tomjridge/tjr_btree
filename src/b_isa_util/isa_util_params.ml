open Iu_pervasives
open Store_ops

type ('k,'v,'r) frame = ('k,'v,'r) Frame.frame
type ('k,'v) tree = ('k,'v) Tree.tree
type ('k,'v,'r,'t) r2t = ('t -> 'r -> ('k,'v) tree option)   (* NOTE on type 't *)

let compare_k x : 'k -> 'k -> int = (x#compare_k)

let constants x : Constants.t = (x#constants)

let store_ops x : ('k,'v,'r,'t) store_ops = (x#store_ops)

(* type ('k,'v,'r,'t) debug_ops = < r2t:('k,'v,'r,'t) r2t > *)

open Yojson.Safe

type ('k,'v,'r,'t,'k2j,'v2j) debug = {
  r2t: ('k,'v,'r,'t) r2t;
  k2j: 'k -> json;
  v2j: 'v -> json;
  r2j: 'r -> json
}

let debug x : ('k,'v,'r,'t,'k2j,'v2j) debug option = (x#debug)

let block_size x : int = (x#block_size)

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
