(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)


open Prelude
open Btree_api
open Functorized_btree

module Make = functor (W: WORLD) -> (struct
    open W
    module Api = Make_api(W)
    open Api

    type page_ref_ops = {
      get_page_ref: unit -> page_ref m;
      set_page_ref: page_ref -> unit m;
    }

    (* produce a ('k,'v) Map.ops, with page_ref state set/get via monad_ops *)
    let make: page_ref_ops -> ('k,'v) kv_ops -> ('k,'v) store_ops -> ('k,'v) map_ops = (
      fun (type k') (type v') page_ref_ops kv_ops store_ops ->
        let 
          (module S: Isa_util.PARAMS 
            with type k = k' and type v = v' and  type store = W.t and type page_ref = int) 
          = 
          (module struct
            type k = k'
            type v = v'
            let compare_k = kv_ops.compare_k
            let equal_v = kv_ops.equal_v
            type store = W.t
            type 'a m = 'a W.m
            type page_ref = int
            let cs0 = store_ops.cs0
            let mk_r2f = store_ops.mk_r2f 
            let store_read = store_ops.store_read
            let store_alloc = store_ops.store_alloc
            let store_free = store_ops.store_free 
          end)
        in
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


