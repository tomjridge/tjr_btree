(** B-tree api types *)

(** This module describes the main interfaces. The interfaces are heavily parameterized. 

To understand the interfaces, we need to introduce the following:

- Keys, represented by type variable ['k]
- Values, by type var ['v]
- Page/block references, ['r]
- Global state, ['t]

The operations typically execute in the {!Base_types.Monad}.


*)

(* various interfaces ---------------------------------------- *)

(* this module safe to open *)

open Base_types
open Prelude
open Default

(* block device ---------------------------------------- *)

(** Disk operations: read, write, and sync *)
type 't disk_ops = {
  block_size: BLK.sz;
  read: BLK.r -> (BLK.t,'t) m;
  write: BLK.r -> BLK.t -> (unit,'t) m;
  disk_sync: unit -> (unit,'t) m;
}


(* store ------------------------------------------------------------ *)

(** Store operations: alloc, free and read. The store is a level above
   the raw disk, on which we build the B-tree. *)
type ('k,'v,'r,'t) store_ops = ('k,'v,'r,'t) Store_ops.store_ops = {
  store_free: 'r list -> (unit,'t) m;
  store_read : 'r -> (('k, 'v,'r) frame,'t) m;  (* FIXME option? *)
  store_alloc : ('k, 'v,'r) frame -> ('r,'t) m;
}

(* map ------------------------------------------------------------ *)

(* TODO insert_many *)

(** Map operations: find, insert, delete *)
type ('k,'v,'t) map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t) m;
}

(** Dummy module containing imperative version of the map interface. *)
module Imperative_map_ops = struct
  (** Imperative version of the map interface; throws exceptions. WARNING likely to change. *)
  type ('k,'v) map_ops = {
    find: 'k -> 'v option;
    insert: 'k -> 'v -> unit;
    delete: 'k -> unit;
  }
end

(* we only reveal lss when it points to a leaf *)

(** Leaf stream representation. This type is for debugging - you
   shouldn't need to access components. *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)Small_step.ls_state }

(** A leaf stream is a linear sequence of the leaves in a B-tree, used
   for iterating over all the bindings in the tree. Leaf stream
   operations: make a leaf stream; get the list of (key,value) pairs
   associated to the state of the leaf stream; step to the next
   leaf. *)
type ('k,'v,'r,'t) ls_ops = {
  mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m;
  ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m;
  ls_kvs: ('k,'v,'r) lss -> ('k*'v) list
}


(* for debugging *)

(** Get all (key,value) pairs from a leaf stream. Debugging only. *)
let all_kvs: ('k,'v,'r,'t)ls_ops -> (('k * 'v) list,'t) m = Monad.(
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

(** Utility module to fix page_ref as int *)
module Page_ref_int = struct
  type page_ref = int  [@@deriving yojson]
  type ('k,'v) frame = ('k,'v,page_ref) Frame.frame
end

