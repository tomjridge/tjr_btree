(* leaf stream ---------------------------------------- *)

(* we need to repeatedly step the leaf state to the point that we hit
   a leaf and dest_LS_leaf <> None; INVARIANT every ls_state
   constructed or exposed here has dest_LS_leaf <> None *)

open Prelude

module IU = Isa_util

open IU

(* we only reveal lss when it points to a leaf *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)ls_state }

let rec next_leaf ps1 lss : (('k,'v,'r) lss option,'t) m = (
  let open Simple_monad in
  match (ls_is_finished lss.ls) with
  | true -> return None
  | false -> (
      lss.ls |> ls_step ps1 |> bind (fun ls' ->
          match (ls_dest_leaf ls') with
          | None -> next_leaf ps1 {lss with ls=ls'}
          | Some kvs -> return (Some {kvs;ls=ls'}))))

let mk_leaf_stream ps1 r : (('k,'v,'r)lss,'t) m = (
  let open Simple_monad in
  let ls = mk_ls_state r in
  match (ls_dest_leaf ls) with
  | None -> (
      (* there must be some leaf *)
      next_leaf ps1 {kvs=[];ls} |> bind (fun lss' -> return (dest_Some lss')))
  | Some kvs -> return {kvs; ls})

let ls_kvs ls : ('k*'v) list = (
  ls |> ls_dest_leaf |> dest_Some)  (* guaranteed <> None *)

let ls_step ps1 ls = next_leaf ls

