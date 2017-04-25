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

(* TODO hide implementation details of this type? *)
type ('k,'v) ls_state = ('k,'v,page_ref) Isa_export.Pre_params.ls_state

type ('k,'v) kv_ops = {
  compare_k: 'k -> 'k -> int;
  equal_v: 'v -> 'v -> bool;
}

(* typically we also are interested in pickling *)
type ('k,'v) kv_params = {
  pp:('k,'v) Pickle_params.t;
  kv_ops: ('k,'v) kv_ops
}

module type WORLD = sig 
  type t
  type 'a m = t -> (t * ('a,string) result) 
  val bind: ('a -> 'b m) -> 'a m -> 'b m
  val return: 'a -> 'a m
end

module Make_world = functor (T:sig type t end) -> struct
  type t = T.t
  type 'a m = ('a,t) Simple_monad.m
  let bind = Simple_monad.bind
  let return = Simple_monad.return
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

    (* just an abstraction, so no sync; use sync on underlying disk *)
    type ('k,'v) store_ops = {
      cs0: constants;
      store_free: page_ref list -> unit m;
      store_read : page_ref -> ('k, 'v) frame m;  (* FIXME option? *)
      store_alloc : ('k, 'v) frame -> page_ref m;
      mk_r2f: t -> page_ref -> ('k,'v) frame option;  (* FIXME remove from Api and provide as extra function param? *)
    }


    (* map ------------------------------------------------------------ *)

    (* FIXME make leafstream ops a different type - not always
       available eg in cached *)

    type ('k,'v) map_ops = {
      find: 'k -> 'v option m;
      insert: 'k -> 'v -> unit m;
      delete: 'k -> unit m;
      mk_leaf_stream: unit -> ('k,'v) ls_state m;
      ls_step: ('k,'v) ls_state -> ('k,'v) ls_state option m;
      ls_kvs: ('k,'v) ls_state -> ('k*'v) list
    }


    (* for debugging *)
    let all_kvs: ('k,'v)map_ops -> ('k * 'v) list m = Simple_monad.(
        fun ops ->
          let rec loop kvs s = (
            let kvs' = ops.ls_kvs s in
            let kvs = kvs@kvs' in
            ops.ls_step s |> bind (fun s' ->
                match s' with
                | None -> return kvs
                | Some s' -> loop kvs s'))
          in
          ops.mk_leaf_stream () |> bind (fun s -> loop [] s))

end)

