(* simple in-mem implementation, mainly for testing ----------------------------- *)

(* NB pages are not simple byte arrays; they are frames; this avoids
   need to fiddle with frame<->page mappings 


   We are parametric over KV and C

*)


open Prelude
open Btree_api

module M = (World : MONAD with type 'a m = 'a World.m)


module type S = sig
  module KV : KEY_VALUE
  module C : Btree.CONSTANTS  
end


module Map_int = Btree_util.Map_int


module Make = functor (S:S) -> (struct

    module S = S

    (* we put these outside the call to Btree_simple_internal.Make, so we can
       have visibility into the types, particularly the store type *)
    module C = S.C
    module KV = S.KV

    module Page_ = struct
      type r = Btree_api.ptr [@@deriving yojson]
    end

    module FT = struct
      open KV
      type pframe =  
          Node_frame of (key list * Page_.r list) |
          Leaf_frame of (key * value) list[@@deriving yojson]
      type page = pframe[@@deriving yojson]

      let frame_to_page : pframe -> page = fun x -> x
      let page_to_frame : page -> pframe = fun x -> x
    end

    module Page = struct
      include Page_
      type t = FT.pframe [@@deriving yojson]
      type fixme
    end

    let _ = (module Page : PAGE_LIKE)

    (* want to call Btree.Make *)
    module ST (* : Btree.STORE *) = struct
      module Page = Page
      open Page
      include M
      open M

      type store = {free:int; m:Page.t Map_int.t}
      type t = store World.r * World.t

      (* for yojson *)
      type store' = {free':int; m':(int * Page.t) list}[@@deriving yojson]

      let store_to_' s = {free'=s.free; m'=s.m|>Map_int.bindings}

      (*
      let dest_Store : store -> page_ref -> page = (
        fun s r -> Map_int.find r s.m)
*)
      let get_store: t -> store m = (
        fun t -> World.get t)

      let put_store: t -> store -> unit m = (
        fun t s -> World.set t s)

      let free: t -> Page.r list -> unit m = (
        fun t rs -> return ())  (* no-op *)

      let alloc: t -> Page.t -> Page.r m = (
        fun t p -> 
          get_store t |> bind (fun s -> 
              let s' = {free=s.free+1; m=Map_int.add s.free p s.m} in
              put_store t s' |> bind (fun () -> 
                return s.free)))

      let page_ref_to_page: t -> Page.r -> Page.t m = (
        fun t r -> get_store t |> bind (fun s ->
            Map_int.find r s.m |> return))

      let store_sync: t -> unit m = (fun t -> return ())  (* no-op *)

    end (* ST *)

    let _ = (module ST : STORE)


    (* want to construct Btree.Main.S in order to call Main.Make *)
    module Btree = Btree.Make(struct 
        module C = C
        module KV = KV
        module FT = FT
        module ST = ST
      end)  (* Btree *)

  end)  (* Make *)


(* example int int btree ---------------------------------------- *)

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

