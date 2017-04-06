(* various interfaces ---------------------------------------- *)

open Prelude

(* monad ---------------------------------------- *)

(*
module type MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: ('a -> 'b m) -> 'a m -> 'b m
end
*)

(* block device ---------------------------------------- *)

type blk_index = int

module type BLK_LIKE = sig
    type t
    type r = blk_index [@@deriving yojson] (* number/index of a block *)
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
end

module Default_block = struct
  type t = string
  type r = int
  type sz = int  (* in bytes *)
end

(* params ---------------------------------------- *)

type constants = Isa_util.constants

(* store ------------------------------------------------------------ *)

type page_ref = int

type 'a m = 'a World.m

type ('k,'v) frame = ('k,'v,page_ref) Frame.t

module type Kv_store = Isa_util.PARAMS

(* want to have poly defns; store type exposed since needed in btree *)
module Store_open = struct
  (* a "store" is actually.. what? *)
  type ('k,'v,'store) t = {
    compare_k: 'k -> 'k -> int;
    equal_v: 'v -> 'v -> bool;
    cs0: constants;
    store_free: page_ref list -> unit m;
    store_read : page_ref -> ('k, 'v) frame m;
    store_alloc : ('k, 'v) frame -> page_ref m;
    mk_r2f: 'store -> page_ref -> ('k,'v) frame option;
    t: 'store World.r
  }
  let store_free: ('k,'v,'store) t -> page_ref list -> unit m = (
    fun s -> s.store_free)
  let store_read: ('k,'v,'store) t -> page_ref -> ('k, 'v) frame m = (
    fun s -> s.store_read)
  let store_alloc : ('k,'v,'store) t -> ('k, 'v) frame -> page_ref m = (
    fun s -> s.store_alloc)
  let mk_r2f = fun s t -> s.mk_r2f t
  let t = fun s -> s.t
end

(*
module Store_closed : sig
  type ('k,'v,'t) t
  val params: ('k,'v,'t) t -> ('k,'v) params
  val store_free: ('k,'v,'t) t -> page_ref list -> unit m
  val store_read: ('k,'v,'t) t -> page_ref -> ('k, 'v) frame m
  val store_alloc : ('k,'v,'t) t -> ('k, 'v) frame -> page_ref m
  val mk_r2f: ('k,'v,'t) t -> 't -> page_ref -> ('k,'v) frame option
  val t: ('k,'v,'t) t -> 't World.r
end = Store_open
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

(* leaf stream ---------------------------------------- *)

type ('k,'v) leaf_stream = {
  step: unit -> bool m;
  get_kvs: unit -> ('k * 'v) list m;
}

(* map ------------------------------------------------------------ *)

(* this exposes the impl type, so users can access the raw impl *)
module Map : sig
  type ('k,'v,'t) t
  val find: ('k,'v,'t) t -> 'k -> 'v option m
  val insert: ('k,'v,'t) t -> ('k * 'v) -> unit m
  val delete: ('k,'v,'t) t -> 'k -> unit m
  val get_leaf_stream: ('k,'v,'t) t -> ('k,'v) leaf_stream m
end = struct
  type ('k,'v,'t) t = {
    find: 'k -> 'v option m;
    insert: ('k * 'v) -> unit m;
    delete: 'k -> unit m;
    get_leaf_stream: unit -> ('k,'v) leaf_stream m;
    t: 't World.r
  }
  let find: ('k,'v,'t) t -> 'k -> 'v option m = (fun m -> m.find)
  let insert: ('k,'v,'t) t -> ('k * 'v) -> unit m = (fun m -> m.insert)
  let delete: ('k,'v,'t) t -> 'k -> unit m = (fun m -> m.delete)
  let get_leaf_stream: ('k,'v,'t) t -> ('k,'v) leaf_stream m = (fun m -> m.get_leaf_stream ())
end


  
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

