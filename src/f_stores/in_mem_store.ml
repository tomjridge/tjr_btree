(* simple in-mem implementation, mainly for testing ----------------------------- *)


open Prelude

module type S = sig
    module W: Btree_api.WORLD
    module Store: Btree_api.Store with module W = W
    type k
    type v
    val compare_k: k -> k -> int
    val equal_v: v -> v -> bool
end

module Make = functor (S:S) -> (struct

  module W = S.W
  open W
  open S

  type frame = (k,v) Btree_api.frame
  type page_ref = Btree_api.page_ref
  type page = frame

  type store = {free:int; map:frame Map_int.t}

  type extra_ops = {
      get_store: unit -> store m;
      set_store: store -> unit m;     
    }


  let make (ops:extra_ops) (cs0:Constants.t) = (
    let store_free: page_ref list -> unit m = (
      fun rs -> return ())  (* no-op *)
    in
    let store_alloc: page -> page_ref m = (fun p -> 
      ops.get_store () |> bind (fun s -> 
      let s' = {free=s.free+1; map=Map_int.add s.free p s.map} in
      ops.set_store s' |> bind (fun () -> 
      return s.free)))
    in
    let store_read: page_ref -> frame m = (fun r ->
      ops.get_store () |> bind (fun s ->
      return (Map_int.find r s.map))) (* FIXME assumes present *)
    in
    let mk_r2f: t -> page_ref -> frame option = (
        fun t r -> 
        let f = ops.get_store () |> bind (fun s ->
                                      Map_int.find r s.map |> return)
        in
        t|>f|>(fun (_,res) -> 
               match res with
               | Ok frm -> Some frm
               | _ -> None))
    in
    (*let store_sync: t -> unit m = (fun t -> return ())  (* no-op *) *)
    Store.{ compare_k;equal_v;cs0;store_free;store_read;store_alloc;mk_r2f}
  )

end)



(* old ============================================================ *)

      (* for yojson *)
                                                 (*
      type store' = {free':int; m':(int * Page.t) list}[@@deriving yojson]

      let store_to_' s = {free'=s.free; m'=s.m|>Map_int.bindings}
                                                  *)
      (*
      let dest_Store : store -> page_ref -> page = (
        fun s r -> Map_int.find r s.m)
*)


(* example int int btree ---------------------------------------- *)

(*
module Example = struct 
  include Make(struct 
      module C : CONSTANTS = struct
        let max_leaf_size = 5
        let max_node_keys = 5
        let min_leaf_size = 2
        let min_node_keys = 2
      end

      module KV (* : KEY_VALUE_TYPES *) = struct 
        type key = int[@@deriving yojson]
        type value = int[@@deriving yojson]
        let key_ord k1 k2 = Pervasives.compare k1 k2
        let equal_value = (=)
      end
    end)

  let empty = Map_int.empty

end

 *)
