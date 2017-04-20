(* various interfaces ---------------------------------------- *)

open Prelude


module Default_block : sig
  type t
  type r = int
  type sz = int  (* of a block, in bytes *)
  val of_string: sz -> string -> t
  val to_string: t -> string
end = struct
  type t = string
  type r = int
  type sz = int 
  let of_string: sz -> string -> t = (
    fun sz s ->
      assert (String.length s <= sz); (* TODO asserts should be Test.assert *)
      s ^ (String.make (sz - String.length s) (Char.chr 0))
  )
  let to_string: t -> string = fun x -> x
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

type ('a,'s) m = ('a,'s) Simple_monad.m

type 's disk_ops = {
  block_size: BLK.sz;
  read: BLK.r -> (BLK.t,'s) m;
  write: BLK.r -> BLK.t -> (unit,'s) m;
  disk_sync: unit -> (unit,'s) m;
}


(* store ------------------------------------------------------------ *)

(* just an abstraction, so no sync? use sync on underlying disk? *)
type ('k,'v,'s) store_ops = {
  compare_k: 'k -> 'k -> int;
  equal_v: 'v -> 'v -> bool;
  cs0: constants;
  store_free: page_ref list -> (unit,'s) m;
  store_read : page_ref -> (('k, 'v) frame,'s) m;  (* FIXME option? *)
  store_alloc : ('k, 'v) frame -> (page_ref,'s) m;
  mk_r2f: 's -> page_ref -> ('k,'v) frame option;
}


(* map ------------------------------------------------------------ *)

type ('k,'v) leaf_stream
type ('k,'v,'s) ls_ops = {
  step: ('k,'v) leaf_stream -> (('k,'v) leaf_stream option,'s) m;
  get_kvs: ('k,'v) leaf_stream -> (('k*'v) list,'s) m
}
type ('k,'v,'s) map_ops = {
  find: 'k -> ('v option,'s) m;
  insert: 'k -> 'v -> (unit,'s) m;
  delete: 'k -> (unit,'s) m;
  get_leaf_stream: unit -> ('k,'v,'s) ls_ops;
}



(* make all types given W ------------------------------------------- *)

module Make = functor (W:WORLD) -> struct
  
  module W = W
  open W

  module Disk = struct
    module W = W
    type ops = {
      block_size: BLK.sz;
      read: BLK.r -> BLK.t m;
      write: BLK.r -> BLK.t -> unit m;
      disk_sync: unit -> unit m;
    }
  end

  let _ = (module Disk : DISK)

  module Store = struct
    module W = W
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

  let _ = (module Store : STORE)


  module Map = struct
    module W = W
    module LS = struct 
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

  let _ = (module Map : MAP)


end
