open Store_ops

type ('k,'v,'r) frame = ('k,'v,'r) Frame.frame
type ('k,'v) tree = ('k,'v) Tree.tree
type ('k,'v,'r,'t) r2t = ('t -> 'r -> ('k,'v) tree option) 

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


let compare_k x : 'k -> 'k -> int = (x#compare_k)

let constants x : Constants.t = (x#constants)

let store_ops x : ('k,'v,'r,'t) store_ops = (x#store_ops)

let r2t x : ('k,'v,'r,'t) r2t option = (x#r2t)

let block_size x : int = (x#block_size)

let page_size = block_size

(*let spec_tree x : ('k,'v) tree option = (x#spec_tree)*)

(* let t_state x : 't = (x#t_state) *)

(*
other common parameters:

r2t tree state 

*)
