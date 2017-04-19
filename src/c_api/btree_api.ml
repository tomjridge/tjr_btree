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


module type WORLD = sig 
  type t
  type 'a m = t -> (t * ('a,string) result) 
  val bind: ('a -> 'b m) -> 'a m -> 'b m
  val return: 'a -> 'a m
end


(* block device ---------------------------------------- *)

module type DISK = sig
  module W : WORLD
  open W
  type ops = {
    block_size: BLK.sz;
    read: BLK.r -> BLK.t m;
    write: BLK.r -> BLK.t -> unit m;
    disk_sync: unit -> unit m;
  }
end


(* store ------------------------------------------------------------ *)

(* just an abstraction, so no sync? use sync on underlying disk? *)
module type STORE = sig
  module W : WORLD
  open W
  type ('k,'v) ops = {
    compare_k: 'k -> 'k -> int;
    equal_v: 'v -> 'v -> bool;
    cs0: constants;
    store_free: page_ref list -> unit m;
    store_read : page_ref -> ('k, 'v) frame m;  (* FIXME option? *)
    store_alloc : ('k, 'v) frame -> page_ref m;
    mk_r2f: t -> page_ref -> ('k,'v) frame option;
  }
end


(* map ------------------------------------------------------------ *)

module type MAP = sig
  module W : WORLD
  open W
  module LS : sig 
    type ('k,'v) leaf_stream
    type ('k,'v) t = ('k,'v) leaf_stream
    type ('k,'v) ops = {
      step: ('k,'v) t -> ('k,'v) t option m;
      get_kvs: ('k,'v) t -> ('k*'v) list m
    }
  end

  type ('k,'v) ops = {
    find: 'k -> 'v option m;
    insert: 'k -> 'v -> unit m;
    delete: 'k -> unit m;
    get_leaf_stream: unit -> ('k,'v) LS.t m;
  }
end
