(* simple in-mem implementation, mainly for testing ----------------------------- *)

(* NB pages are not simple byte arrays; they are frames; this avoids
   need to fiddle with frame<->page mappings 


   We are parametric over KV and C

*)



(* setup ---------------------------------------- *)

open Btree


module type S = sig

  module KV : Btree.KEY_VALUE_TYPES
  module C : Btree.CONSTANTS  

end


module Map_int = Btree_util.Map_int


module Make = functor (S:S) -> struct

  module S = S

  (* we put these outside the call to Btree_simple.Make, so we can
     have visibility into the types, particularly the store type *)
  module C = S.C
  module KV = S.KV

  module PR = struct 
    type page_ref = int[@@deriving yojson]
  end

  module FT = struct
    open KV
    open PR
    type pframe =  
        Node_frame of (key list * page_ref list) |
        Leaf_frame of (key * value) list[@@deriving yojson]

    type page = pframe[@@deriving yojson]

    let frame_to_page : pframe -> page = fun x -> x
    let page_to_frame : page -> pframe = fun x -> x

  end

  module ST = struct

    type page = FT.page  [@@deriving yojson]
    type page_ref = PR.page_ref  [@@deriving yojson]
    type store = {free:int; m:page Map_int.t}

    type 'a m = ('a,store) Btree_api.State_error_monad.m

    (* for yojson *)
    type store' = {free':int; m':(int * page) list}[@@deriving yojson]

    let store_to_' s = {free'=s.free; m'=s.m|>Map_int.bindings}

    let dest_Store : store -> page_ref -> page = (
      fun s r -> Map_int.find r s.m)

    let page_ref_to_page: page_ref -> page m = (
      fun r -> (fun s -> (s,Ok(Map_int.find r s.m))))

    let alloc: page -> page_ref m = (
      fun p -> (fun s ->
          let f = s.free in
          ({free=(f+1);m=Map_int.add f p s.m}),Ok(f)))

    let free: page_ref list -> unit m = (
      fun ps -> (fun s -> (s,Ok(()))))

  end (* ST *)

  let _ = (module ST : Btree_api.STORE)


  (* want to construct Btree.Main.S in order to call Main.Make *)
  module Btree = Btree.Main.Make(struct 
      module C = C
      module KV = KV
      module FT = FT
      module ST = ST
    end)  (* Btree *)

end  (* Make *)


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

