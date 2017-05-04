(** The essential B-tree functionality: implement a map on top of a store. *)


(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)

open Base_types
open Prelude
open Btree_api
open Page_ref_int  (* TODO generalize? *)

(** The B-tree code exports a [pre_map_ops] version of a map, with
   explicit passing of the reference to the root of the B-tree. In
   order to implement the [map_ops] interface, we need to store the
   "current" reference to the B-tree root in the global state
   somehow. A value of type ['t page_ref_ops] reveals how to read and
   write this reference in the global state. *)
type 't page_ref_ops = {
  get_page_ref: unit -> (page_ref,'t) m;
  set_page_ref: page_ref -> (unit,'t) m;
}

open Monad
open Pre_map_ops

(* produce a map, with page_ref state set/get via monad_ops *)
let make_map_ops' pre_map_ops page_ref_ops : ('k,'v,'t) map_ops = (
  let find = (fun k ->
      page_ref_ops.get_page_ref () |> bind (fun r ->
          pre_map_ops.find k r |> bind (fun (r',kvs) -> 
              (* page_ref_ops.set_page_ref r' |> bind (fun () -> NO! the r is the pointer to the leaf *)
                  return (try Some(List.assoc k kvs) with _ -> None))))
  in
  let insert = (fun k v ->
      page_ref_ops.get_page_ref () |> bind (fun r ->
          pre_map_ops.insert k v r |> bind (fun r' -> 
              page_ref_ops.set_page_ref r')))
  in
  let delete = (fun k ->
      page_ref_ops.get_page_ref () |> bind (fun r -> 
          pre_map_ops.delete k r |> bind (fun r' ->
              page_ref_ops.set_page_ref r')))
  in
  Btree_api.{find; insert; delete })

(** Make [map_ops], given a [page_ref_ops]. TODO make store_ops
   explicit in arguments to this function *)
let make_map_ops ps page_ref_ops = 
  Iter_with_check.make_pre_map_ops ps
  |> fun x -> make_map_ops' x page_ref_ops


let map_ops_to_imperative_map_ops (map_ops:('k,'v,'t)Btree_api.map_ops) (s_ref:'t ref) = (
  let find k = map_ops.find k |> run (!s_ref) |> fun (s',Ok res) -> (s_ref:=s'; res) in
  let insert k v = map_ops.insert k v |> run (!s_ref) |> fun (s',Ok res) -> (s_ref:=s'; res) in
  let delete k = map_ops.delete k |> run (!s_ref) |> fun (s',Ok res) -> (s_ref:=s'; res) in
  Imperative_map_ops.{find;insert;delete}
)

module N = Iter_leaf_stream

(** Make [ls_ops], given a [page_ref_ops]. TODO ditto *)
let make_ls_ops ps page_ref_ops : ('k,'v,'r,'t) ls_ops = (
  let mk_leaf_stream = (fun () ->
      page_ref_ops.get_page_ref () |> bind (fun r -> 
          N.mk_leaf_stream ps r))
  in
  let ls_step = N.ls_step ps in
  let ls_kvs = N.ls_kvs in
  { mk_leaf_stream; ls_step; ls_kvs }
)

