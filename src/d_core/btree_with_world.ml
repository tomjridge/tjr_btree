(* refine btree to use world monad ---------------------------------------- *)

(* and output Btree_api.{MAP,LEAF_STREAM} *)

open Prelude
open Btree_api

module type STORE = sig
  module Page : PAGE_LIKE
  open Page
  type store
  type 'a m = 'a World.m  (* the monad is the world monad *)
  type t = store World.r
  val r: t  (* the store we are interested in *)
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

(* make a Btree.STORE *)
module Make_btree_store = functor (ST:STORE) -> struct
  module R_ = struct
    module Page = ST.Page
    open Page
    type t = World.t  (* but the point is that we want this to be world.t *)
    type 'a m = ('a,t) Sem.m
    type store = ST.store
    let get_store: unit -> store m = (fun () -> World.get ST.r)
    let set_store: store -> unit m = (fun s -> World.set ST.r s)
    let free: r list -> unit m = (fun rs -> ST.free ST.r rs)
    let alloc: Page.t -> r m = (fun p -> ST.alloc ST.r p)
    let page_ref_to_page: r -> Page.t m = (fun r -> ST.page_ref_to_page ST.r r)
    let store_sync: unit m = (ST.store_sync ST.r)
    let bind = World.bind
    let return = World.return
  end
  let _ = (module R_ : Btree.STORE)
  include R_
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
module Make_btree_s = functor (S:S) -> (struct
    module R_ = struct
      (* need to call Btree.Make; ST needs to be modified *)
      module S = S
      module C = S.C
      module KV = S.KV
      module ST = Make_btree_store(S.ST)
      module FT = S.FT
    end
    let _ = (module R_ : Btree.S) 
    include R_
  end)


module Make = functor (S:S) -> struct
  module S = S
  module S_ = Make_btree_s(S)
  module Btree = Btree.Make(S_)
end
  
