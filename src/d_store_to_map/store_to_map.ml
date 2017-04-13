(* map_ops ---------------------------------------- *)

(* instead of btree_make, use records *)

(* we want to get versions which use records rather than functors *)


(* different ops address different maps etc, and store page_ref in different places *)

open Functorized_btree

module type S = sig
    module Store: Btree_api.Store
    module Map: Btree_api.Map with module W = Store.W
end

open Btree_api

module Page_ref_ops = struct
    type 'w t = {
        get_page_ref: 'w -> page_ref;
        set_page_ref: page_ref -> 'w -> 'w;
      }
end

module Make = functor (S: S) -> (struct
    open Btree_api
    module W = S.Store.W
    module Store = S.Store
    module Map = S.Map                 
    open W

    type page_ref_ops = W.t Page_ref_ops.t
    open Page_ref_ops

    (* produce a ('k,'v) Map.ops, with page_ref state set/get via monad_ops *)
    let make: page_ref_ops -> ('k,'v) Store.ops -> ('k,'v) Map.ops = (
      fun (type k') (type v') ops s ->
      let 
        (module S: Isa_util.PARAMS 
          with type k = k' and type v = v' and  type store = W.t and type page_ref = int) 
        = 
        (module struct
          type k = k'
          type v = v'
          let compare_k = Store.(s.compare_k)
          let equal_v = s.equal_v
          type store = W.t
          type 'a m = store -> (store * ('a,string)result)
          type page_ref = int
          let cs0 = s.cs0
          let mk_r2f = s.mk_r2f 
          let store_read = s.store_read
          let store_alloc = s.store_alloc
          let store_free = s.store_free 
        end) 
      in
      (* let (module M : RR with module P = S) = (module Make_functor.Make(S)) in *)
      let 
        (module M : Make_functor.RR 
          with type P.k = k' and type P.v = v' and type P.store = W.t and type P.page_ref = int)
        = 
        (module Make_functor.Make(S))
      in
      let find: k' -> v' option m = fun k w ->
        let r = ops.get_page_ref w in
        M.find k r w |> (fun (w',res) -> 
            match res with
            | Ok (r',kvs) -> (w'|>ops.set_page_ref r', Ok (try Some(List.assoc k kvs) with _ -> None))
            | Error e -> (w', Error e))
      in
      let insert: k' -> v' -> unit m = fun k v w ->
        let r = ops.get_page_ref w in
        M.insert k v r w |> (fun (w',res) -> 
            match res with
            | Ok (r') -> (w'|>ops.set_page_ref r', Ok ())
            | Error e -> (w', Error e))
      in
      let delete: k' -> unit m = fun k w ->
        let r = ops.get_page_ref w in
        M.delete k r w |> (fun (w',res) -> 
            match res with
            | Ok (r') -> (w'|>ops.set_page_ref r', Ok ())
            | Error e -> (w', Error e))
      in
      let get_leaf_stream: unit -> (k',v') Map.LS.t m = fun () w ->
        let r = ops.get_page_ref w in
        failwith "FIXME"
      in
      Map.{find; insert; delete; get_leaf_stream})

  

end)


