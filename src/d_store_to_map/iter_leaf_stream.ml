(** Wrap the small-step leaf-stream operations. *)

(* we need to repeatedly step the leaf state to the point that we hit
   a leaf and dest_LS_leaf <> None; INVARIANT every ls_state
   constructed or exposed here has dest_LS_leaf <> None *)

open Base_types
open Small_step
open Tjr_step_monad
open Params
open Leaf_stream_ops

let ils_mk ~ps ~store_ops = (

  let rec next_leaf lss : (('k,'v,'r) lss option,'t) m = (
    match (ls_is_finished lss.ls) with
    | true -> return None
    | false -> (
        lss.ls 
        |> ls_step ~constants:(constants ps) ~cmp:(cmp ps) ~store_ops 
        |> bind @@ fun ls' ->
        match (ls_dest_leaf ls') with
        | None -> next_leaf {lss with ls=ls'}
        | Some kvs -> return (Some {kvs;ls=ls'})))
  in

  let mk_leaf_stream r : (('k,'v,'r)lss,'t) m = (
    let ls = mk_ls_state r in
    match (ls_dest_leaf ls) with
    | None -> (
        (* at root, which is not a leaf; there must be some leaf *)
        next_leaf {kvs=[];ls} |> bind (fun lss' -> return (dest_Some lss')))
    | Some kvs -> return {kvs; ls})
  in

  let ls_kvs ls : ('k*'v) list = ls.kvs in

  let ls_step ls = next_leaf ls in

  fun k -> k ~mk_leaf_stream ~ls_kvs ~ls_step)

