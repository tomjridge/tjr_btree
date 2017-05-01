(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)


open Prelude
open Btree_api
open Page_ref_int  (* TODO generalize? *)

type 't page_ref_ops = {
  get_page_ref: unit -> (page_ref,'t) m;
  set_page_ref: page_ref -> (unit,'t) m;
}

open Simple_monad
open Pre_map_ops

(* produce a map, with page_ref state set/get via monad_ops *)
let make_map_ops' pre_map_ops page_ref_ops : ('k,'v,'t) map_ops = (
  let find = (fun k ->
      page_ref_ops.get_page_ref () |> bind (fun r ->
          pre_map_ops.find k r |> bind (fun (r',kvs) -> 
              page_ref_ops.set_page_ref r' |> bind (fun () ->
                  return (try Some(List.assoc k kvs) with _ -> None)))))
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

let make_map_ops ps page_ref_ops = 
  Iter_with_check.make_pre_map_ops ps
  |> fun x -> make_map_ops' x page_ref_ops

module N = Iter_leaf_stream

let make_ls_ops ps page_ref_ops : ('k,'v,'r,'t) ls_ops = (
  let mk_leaf_stream = (fun () ->
      page_ref_ops.get_page_ref () |> bind (fun r -> 
          N.mk_leaf_stream ps r))
  in
  let ls_step = N.ls_step ps in
  let ls_kvs = N.ls_kvs in
  { mk_leaf_stream; ls_step; ls_kvs }
)

