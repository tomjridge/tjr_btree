(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)


open Prelude
open Btree_api

type page_ref = int

type 't page_ref_ops = {
  get_page_ref: unit -> (page_ref,'t) m;
  set_page_ref: page_ref -> (unit,'t) m;
}

open Simple_monad
module M = Iter_with_check
module N = Iter_leaf_stream

(* TODO another interface without r2f - no checking *)

(* produce a map, with page_ref state set/get via monad_ops *)
let make_map_ops' ps1 r2t page_ref_ops : ('k,'v,'t) map_ops = (
  let find = (fun k ->
      page_ref_ops.get_page_ref () |> bind (fun r ->
          M.find ps1 r2t k r |> bind (fun (r',kvs) -> 
              page_ref_ops.set_page_ref r' |> bind (fun () ->
                  return (try Some(List.assoc k kvs) with _ -> None)))))
  in
  let insert = (fun k v ->
      page_ref_ops.get_page_ref () |> bind (fun r ->
          M.insert ps1 r2t k v r |> bind (fun r' -> 
              page_ref_ops.set_page_ref r')))
  in
  let delete = (fun k ->
      page_ref_ops.get_page_ref () |> bind (fun r -> 
          M.delete ps1 r2t k r |> bind (fun r' ->
              page_ref_ops.set_page_ref r')))
  in
  {find; insert; delete })

let make_checked_map_ops ps1 r2t page_ref_ops = 
  make_map_ops' ps1 (Some r2t) page_ref_ops

let make_unchecked_map_ops ps1 page_ref_ops =
  make_map_ops' ps1 None page_ref_ops

let make_ls_ops ps1 page_ref_ops : ('k,'v,'r,'t) ls_ops = (
  let mk_leaf_stream = (fun () ->
      page_ref_ops.get_page_ref () |> bind (fun r -> 
          N.mk_leaf_stream ps1 r))
  in
  let ls_step = N.ls_step ps1 in
  let ls_kvs = N.ls_kvs in
  { mk_leaf_stream; ls_step; ls_kvs }
)

