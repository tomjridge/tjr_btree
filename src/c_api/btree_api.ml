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


type ('a,'t) m = ('a,'t) Simple_monad.m


type 'k ord = 'k -> 'k -> int


(*
(* typically we also are interested in pickling *)
type ('k,'v) kv_params = {
  pp:('k,'v) Pickle_params.t;
  compare_k: 'k ord
}
*)


(* block device ---------------------------------------- *)

type 't disk_ops = {
  block_size: BLK.sz;
  read: BLK.r -> (BLK.t,'t) m;
  write: BLK.r -> BLK.t -> (unit,'t) m;
  disk_sync: unit -> (unit,'t) m;
}


(* store ------------------------------------------------------------ *)

open Frame

(* just an abstraction, so no sync; use sync on underlying disk *)
type ('k,'v,'r,'t) store_ops = {
  cs0: Constants.t; (* FIXME belongs here? *)
  store_free: 'r list -> (unit,'t) m;
  store_read : 'r -> (('k, 'v,'r) frame,'t) m;  (* FIXME option? *)
  store_alloc : ('k, 'v,'r) frame -> ('r,'t) m;
}


(* map ------------------------------------------------------------ *)

type ('k,'v,'t) map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t) m;
}


(* we only reveal lss when it points to a leaf *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)Isa_util.ls_state }

type ('k,'v,'r,'t) ls_ops = {
  mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m;
  ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m;
  ls_kvs: ('k,'v,'r) lss -> ('k*'v) list
}


(* for debugging *)
let all_kvs: ('k,'v,'r,'t)ls_ops -> (('k * 'v) list,'t) m = Simple_monad.(
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


(* fix page_ref --------------------------------------------------- *)

type page_ref = int

type ('k,'v) frame = ('k,'v,page_ref)Frame.frame
