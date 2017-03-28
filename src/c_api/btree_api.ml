(* various interfaces ---------------------------------------- *)

open Prelude

module type KEY_VALUE = Prelude.KEY_VALUE

(* monad ---------------------------------------- *)

module type MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: ('a -> 'b m) -> 'a m -> 'b m
end


(* block device ---------------------------------------- *)

type ptr = int [@@deriving yojson]

module type BLK_LIKE = sig
    type t
    type r = ptr [@@deriving yojson] (* number/index of a block *)
    val sz: int  (* size of a block in bytes *)
    val of_string: string -> (t,string) result
    val empty: unit -> t
  end


module type DISK = sig
  module BLK : BLK_LIKE
  open BLK
  type 'a m
  type t
  val read: t -> r -> BLK.t m
  val write: t -> r -> BLK.t -> unit m  
  val disk_sync: t -> unit m
  include MONAD with type 'a m := 'a m
end



(* store ------------------------------------------------------------ *)

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



(* map ------------------------------------------------------------ *)

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


(* leaf stream ---------------------------------------- *)

module type LEAF_STREAM = sig
  module KV : KEY_VALUE
  open KV
  type 'a m
  type t
  val step: t -> bool m
  val get_kvs: t -> (key * value) list m
end


