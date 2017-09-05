open Monad

(* leaf stream ------------------------------------------------------ *)


(* we only reveal lss when it points to a leaf *)

(** Leaf stream representation. This type is for debugging - you
   shouldn't need to access components. *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)Small_step.ls_state }

(** A leaf stream is a linear sequence of the leaves in a B-tree, used
   for iterating over all the bindings in the tree. Leaf stream
   operations: make a leaf stream; get the list of (key,value) pairs
   associated to the state of the leaf stream; step to the next
   leaf. *)
(*
type ('k,'v,'r,'t) ls_ops = {
  mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m;
  ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m;
  ls_kvs: ('k,'v,'r) lss -> ('k*'v) list
}
*)

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


(* for debugging *)

(** Get all (key,value) pairs from a leaf stream. Debugging only. *)
let all_kvs ~ls_ops : (('k * 'v) list,'t) m = Monad.(
    dest_ls_ops ls_ops @@ fun ~mk_leaf_stream ~ls_step ~ls_kvs ->
    let rec loop kvs s = (
      let kvs' = ls_kvs s in
      let kvs = kvs@kvs' in
      ls_step s |> bind (fun s' ->
          match s' with
          | None -> return kvs
          | Some s' -> loop kvs s'))
    in
    mk_leaf_stream () |> bind (fun s -> loop [] s))


