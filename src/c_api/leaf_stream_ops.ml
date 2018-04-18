(** A leaf stream is a linear sequence of the leaves in a B-tree, used
    for iterating over all the bindings in the tree. Leaf stream
    operations: make a leaf stream; get the list of (key,value) pairs
    associated to "current" leaf; step to the next leaf. *)

open Tjr_step_monad
open Ls_state

(* leaf stream ------------------------------------------------------ *)

(* we only reveal lss when it points to a leaf *)

module Leaf_stream_types = struct
  (** Leaf stream representation. This type is for debugging - you
      shouldn't need to access components. *)
  type ('k,'v,'r) leaf_stream_state = { kvs: ('k*'v) list; ls: ('k,'v,'r)ls_state }

  type ('k,'v,'r) lss = ('k,'v,'r) leaf_stream_state

  type ('k,'v,'r,'t) leaf_stream_ops = {
    mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m;
    ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m;
    ls_kvs: ('k,'v,'r) lss -> ('k*'v) list;
  }
    
end
include Leaf_stream_types


let wf_ls_ops 
    ~(mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m)
    ~(ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m)
    ~(ls_kvs: ('k,'v,'r) lss -> ('k*'v) list)
  =
  true

let mk_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs =
  assert(wf_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs);
  { mk_leaf_stream; ls_step; ls_kvs }


let dest_ls_ops { mk_leaf_stream;ls_step; ls_kvs} = 
  assert(wf_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs);
  fun k -> k ~mk_leaf_stream ~ls_step ~ls_kvs

