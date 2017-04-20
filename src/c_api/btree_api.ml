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

module Make_api = functor (W:WORLD) -> (struct

    module W = W
    open W

    (* block device ---------------------------------------- *)

    type disk_ops = {
      block_size: BLK.sz;
      read: BLK.r -> BLK.t m;
      write: BLK.r -> BLK.t -> unit m;
      disk_sync: unit -> unit m;
    }


    (* store ------------------------------------------------------------ *)

    (* just an abstraction, so no sync? use sync on underlying disk? *)
    type ('k,'v) store_ops = {
      compare_k: 'k -> 'k -> int;
      equal_v: 'v -> 'v -> bool;
      cs0: constants;
      store_free: page_ref list -> unit m;
      store_read : page_ref -> ('k, 'v) frame m;  (* FIXME option? *)
      store_alloc : ('k, 'v) frame -> page_ref m;
      mk_r2f: t -> page_ref -> ('k,'v) frame option;
    }


    (* map ------------------------------------------------------------ *)

    module LS = struct
      type ('k,'v) leaf_stream (* TODO *)
      type ('k,'v) ls_ops = {
        step: ('k,'v) leaf_stream -> ('k,'v) leaf_stream option m;
        get_kvs: ('k,'v) leaf_stream -> ('k*'v) list m
      }
    end
    open LS
    type ('k,'v) map_ops = {
      find: 'k -> 'v option m;
      insert: 'k -> 'v -> unit m;
      delete: 'k -> unit m;
      get_leaf_stream: unit -> ('k,'v) ls_ops;
    }


end)

(*
type ('k,'v) kv_params = {
  compare_k: 'k -> 'k -> int;
  equal_v: 'v -> 'v -> bool;
}
*)
