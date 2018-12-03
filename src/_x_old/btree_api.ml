(* various interfaces ---------------------------------------- *)

open Prelude


module Default_block = struct
  type t = string
  type r = int
  type sz = int  (* of a block, in bytes *)
end

module BLK = Default_block


type constants = Isa_util.constants
type page_ref = int
type ('k,'v) frame = ('k,'v,page_ref) Frame.t


module Make = functor (W:sig type 'a m end) -> (struct
    open W

    (* block device ---------------------------------------- *)

    module Disk = struct
      type ops = {
        block_size: BLK.sz;
        read: BLK.r -> BLK.t m;
        write: BLK.r -> BLK.t -> unit m;
        disk_sync: unit -> unit m;
      }
    end


    (* store ------------------------------------------------------------ *)

    module Store = struct
      type ('k,'v) ops = {
        compare_k: 'k -> 'k -> int;
        equal_v: 'v -> 'v -> bool;
        cs0: constants;
        store_free: page_ref list -> unit m;
        store_read : page_ref -> ('k, 'v) frame m;
        store_alloc : ('k, 'v) frame -> page_ref m;
        mk_r2f: unit -> page_ref -> ('k,'v) frame option m;
      }
    end


    (* map ------------------------------------------------------------ *)

    module Leaf_stream = struct 
      type ('k,'v) leaf_stream
      type ('k,'v) t = ('k,'v) leaf_stream
      type ('k,'v) ops = {
        step: ('k,'v) t -> ('k,'v) t option m;
        get_kvs: ('k,'v) t -> ('k*'v) list m
      }
    end

    module LS = Leaf_stream

    module Map = struct
      type ('k,'v,'store) map = {
        find: 'k -> 'v option m;
        insert: ('k * 'v) -> unit m;
        delete: 'k -> unit m;
        get_leaf_stream: unit -> ('k,'v) LS.t m;
      }
    end

  end)



(* old ============================================================ *)


(*
module type MAP = sig
  module KV : KEY_VALUE
  open KV

  type 'a m
  type t
  val find: t -> key -> value option m
  val insert: t -> (key * value) -> unit m
  val delete: t -> key -> unit m
  include MONAD with type 'a m := 'a m
end
*)

(*
module type LEAF_STREAM = sig
  module KV : KEY_VALUE
  open KV
  type 'a m
  type t
  val step: t -> bool m
  val get_kvs: t -> (key * value) list m
end
*)



(*
(* this is the storage level immediately below btree; needs a notion
   of a free list; otherwise, this also makes clear that pages are not
   rewritten; maintains free list, but doesn't try to do anything
   fancy *)

module type PAGE_LIKE = sig
  type t
  type r = ptr [@@deriving yojson]
end


module type STORE = sig
  module Page : PAGE_LIKE
  open Page
  type 'a m 
  type t
  val free: t -> r list -> unit m  (* free list *)
  val alloc: t -> Page.t -> r m
  val page_ref_to_page: t -> r -> Page.t m
  val store_sync: t -> unit m
  include MONAD with type 'a m := 'a m
end
*)
