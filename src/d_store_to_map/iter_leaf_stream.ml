(** Wrap the small-step leaf-stream operations. *)

(* we need to repeatedly step the leaf state to the point that we hit
   a leaf and dest_LS_leaf <> None; INVARIANT every ls_state
   constructed or exposed here has dest_LS_leaf <> None; FIXME do we
   want to introduce a new type for this? *)

open Base_types
open Params
open Isa_btree
open Leaf_stream_ops

let store_ops_to_ls_ops ~monad_ops ~constants ~cmp ~store_ops ~ls_step =

  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in

  let rec next_leaf lss : (('k,'v,'r) lss option,'t) m = (
    match (ls_is_finished lss.ls) with
    | true -> return None
    | false -> (
        lss.ls 
        |> ls_step ~constants ~cmp ~store_ops >>= fun ls' ->
        match (ls_dest_leaf ls') with
        | None -> next_leaf {lss with ls=ls'}
        | Some kvs -> return (Some {kvs;ls=ls'})))
  in

  let mk_leaf_stream r : (('k,'v,'r)lss,'t) m = (
    let ls = mk_ls_state r in
    match (ls_dest_leaf ls) with
    | None -> (
        (* at root, which is not a leaf; there must be some leaf *)
        next_leaf {kvs=[];ls} >>= (fun lss' -> return (dest_Some lss')))
    | Some kvs -> return {kvs; ls})
  in

  let ls_kvs ls : ('k*'v) list = ls.kvs in

  let ls_step ls = next_leaf ls in

  mk_ls_ops ~mk_leaf_stream ~ls_kvs ~ls_step

