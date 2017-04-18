(* convert store to map ---------------------------------------- *)

(* instead of btree_make, use records; we want to get versions which
   use records rather than functors *)


open Prelude
open Btree_api
open Functorized_btree

module type S = sig
  module W : WORLD
  module Store: Btree_api.STORE with module W = W
  module Map: Btree_api.MAP with module W = W
end

module Make = functor (S: S) -> (struct
    module S = S
    module W = S.W
    module Store = S.Store
    module Map = S.Map                 
    open W

    (* different ops address different maps etc, and store page_ref in different places *)
    type page_ref_ops = {
      get_page_ref: unit -> page_ref m;
      set_page_ref: page_ref -> unit m;
    }

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
        let find: k' -> v' option m = (fun k ->
            ops.get_page_ref () |> bind (fun r ->
                M.find k r |> bind (fun (r',kvs) -> 
                    ops.set_page_ref r' |> bind (fun () ->
                        return (try Some(List.assoc k kvs) with _ -> None)))))
        in
        let insert: k' -> v' -> unit m = (fun k v ->
            ops.get_page_ref () |> bind (fun r ->
                M.insert k v r |> bind (fun r' -> 
                    ops.set_page_ref r')))
        in
        let delete: k' -> unit m = (fun k ->
            ops.get_page_ref () |> bind (fun r -> 
                M.delete k r |> bind (fun r' ->
                    ops.set_page_ref r')))
        in
        let get_leaf_stream: unit -> (k',v') Map.LS.t m = fun () ->
          failwith "FIXME"
        in
        Map.{find; insert; delete; get_leaf_stream})

  end)


