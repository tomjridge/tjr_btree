(** A leaf stream is a linear sequence of the leaves in a B-tree, used
   for iterating over all the bindings in the tree. Leaf stream
   operations: make a leaf stream; get the list of (key,value) pairs
   associated to the state of the leaf stream; step to the next
   leaf. *)

open Monad
open Ls_state

(* leaf stream ------------------------------------------------------ *)


(* we only reveal lss when it points to a leaf *)

(** Leaf stream representation. This type is for debugging - you
   shouldn't need to access components. *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)ls_state }


let wf_ls_ops 
    ~(mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m)
    ~(ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m)
    ~(ls_kvs: ('k,'v,'r) lss -> ('k*'v) list)
  =
  true


let mk_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs =
  assert(wf_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs);
  `Ls_ops(mk_leaf_stream,ls_step,ls_kvs)


let dest_ls_ops (`Ls_ops(mk_leaf_stream,ls_step,ls_kvs)) = 
  assert(wf_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs);
  fun k -> k ~mk_leaf_stream ~ls_step ~ls_kvs

