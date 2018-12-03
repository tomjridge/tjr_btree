module type MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: ('a -> 'b m) -> 'a m -> 'b m
end


(* disk ---------------------------------------- *)

module type DISK = sig
  module BLK : sig
    type t
    type idx
    val sz: int
    val string_to_blk: string -> (t,string) result
    val empty: unit -> t
  end
  open BLK
  type 'a m
  type t
  val read: t -> idx -> BLK.t m
  val write: t -> idx * BLK.t -> unit m  
  val disk_sync: t -> unit m
  include MONAD with type 'a m := 'a m
end

(* this is pretty clear; no notion of free list  *)

module type BLOCK_CACHE = DISK  (* but with a cache *)


(* file store etc ---------------------------------------- *)

(* this is the basic level on which the btree functions; needs a
   notion of a free list; otherwise, this also makes clear that pages are
   not rewritten *)
(* maintains free list, but doesn't try to do anything fancy *)
module type STORE = sig
  type page_ref
  type page
  type 'a m
  type t
  val free: t -> page_ref list -> unit m  (* free list *)
  val alloc: t -> page -> page_ref m
  val page_ref_to_page: t -> page_ref -> page m
  val store_sync: t -> unit m
  include MONAD with type 'a m := 'a m
end

(* FIXME what is the right distinction between store and recycling?
   store does not have a free operation? or an alloc_block operation? *)
(* this is like store, but uses free to quickly recycle blocks that
   are not synced *)
(* as store
module type Recycling_store = sig  
  module Store: STORE
  module Disk: DISK with type 'a m = 'a Store.m
end
*)

(* kv etc ---------------------------------------- *)

module KV = struct
  type key
  type value
end

module type MAP = sig
  open KV
  type 'a m
  type t
  (* this form of monad has the disadvantage that we now need to
     interpret a bind recursively; but at least that can be hidden
     from the user *)
  val find: t -> key -> value option m
  val insert: t -> (key * value) -> unit m
  val delete: t -> key -> unit m
  include MONAD with type 'a m := 'a m
end


module type LEAF_STREAM = sig
  open KV
  type 'a m
  type t
  val step: t -> bool m
  val get_kvs: t -> (key * value) list m
end


module type MAP' = sig
  type 'a m 
  type ('key,'value) map

  val insert: ('key,'value) map -> 'key -> 'value -> unit m
  val insert_many: 
    ('key,'value) map -> 'key -> 'value -> ('key*'value) list -> unit m
  val find: ('key,'value) map -> 'key -> 'value option m
  val delete: ('key,'value) map -> 'key -> unit m

end




(* summary: store_ops and disk are the two interfaces at the lower
   level; basic_ops is the interface at the higher level *)
