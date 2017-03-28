(* refine btree to use world monad ---------------------------------------- *)

(* and output Btree_api.{MAP,LEAF_STREAM} *)

open Prelude
open Btree_api

module type STORE = sig
  module Page : PAGE_LIKE
  open Page
  type store
  type 'a m = 'a World.m
  type t = store World.r
  val free: t -> r list -> unit m  (* free list *)
  val alloc: t -> Page.t -> r m
  val page_ref_to_page: t -> r -> Page.t m
  val store_sync: t -> unit m
  include MONAD with type 'a m := 'a m
end

(* above refines Btree_api.STORE *)
module X_ = functor (S_:STORE) -> struct
  let _ = (module S_: Btree_api.STORE)
end

module type S = sig
  module C : Btree.CONSTANTS
  module KV: KEY_VALUE
  module ST: STORE
  (* this module's types depend on the previous modules *)
  module FT : sig
    open KV
    open ST
    type pframe =  
        Node_frame of (key list * ST.Page.r list) |
        Leaf_frame of (key * value) list[@@deriving yojson]
    val frame_to_page : pframe -> ST.Page.t
    val page_to_frame : ST.Page.t -> pframe
  end
end

(* this simply constructs the arguments which can then be given to Btree.Make *)
module Make = functor (S:S) -> (struct

    (* need to call Btree.Make; ST needs to be modified *)
    module S = S
    module C = S.C
    module KV = S.KV
    module ST = struct

    end

end)
