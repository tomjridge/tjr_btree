(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)


open Prelude
open Btree_api


type 't page_ref_ops = {
  get_page_ref: unit -> (page_ref,'t) m;
  set_page_ref: page_ref -> (unit,'t) m;
}

let iter ps1 : 

(* produce a ('k,'v) Map.ops, with page_ref state set/get via monad_ops *)
let make ps1 page_ref_ops : ('k,'v,'t) map_ops = (
    (* let (module M : RR with module P = S) = (module Make_functor.Make(S)) in *)
    let 
      (module M : Make_functor.RR 
        with type P.k = k' and type P.v = v' and type P.store = W.t and type P.page_ref = int)
      = 
      (module Make_functor.Make(S))
    in
    let find: k' -> v' option m = (fun k ->
        page_ref_ops.get_page_ref () |> bind (fun r ->
            M.find k r |> bind (fun (r',kvs) -> 
                page_ref_ops.set_page_ref r' |> bind (fun () ->
                    return (try Some(List.assoc k kvs) with _ -> None)))))
    in
    let insert: k' -> v' -> unit m = (fun k v ->
        page_ref_ops.get_page_ref () |> bind (fun r ->
            M.insert k v r |> bind (fun r' -> 
                page_ref_ops.set_page_ref r')))
    in
    let delete: k' -> unit m = (fun k ->
        page_ref_ops.get_page_ref () |> bind (fun r -> 
            M.delete k r |> bind (fun r' ->
                page_ref_ops.set_page_ref r')))
    in
    let mk_leaf_stream: unit -> (k',v') ls_state m = (fun () ->
        page_ref_ops.get_page_ref () |> bind (fun r -> 
            M.mk_leaf_stream r))
    in
    let ls_step: (k',v') ls_state -> (k',v') ls_state option m = M.ls_step in
    let ls_kvs: (k',v') ls_state -> (k' * v') list = M.ls_kvs in
    Map.{find; insert; delete; mk_leaf_stream; ls_step; ls_kvs})

end)

