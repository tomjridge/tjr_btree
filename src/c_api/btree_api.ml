(** B-tree api types *)

(** This module describes the main interfaces. The interfaces are heavily parameterized. 

To understand the interfaces, we need to introduce the following:

- Keys, represented by type variable ['k]
- Values, by type var ['v]
- Page/block references, ['r]; vars [blk_id]
- Global state, ['t]

The operations typically execute in the {!Base_types.Monad}.


*)


(* FIXME tidy this file by removing commented code *)

(* various interfaces ---------------------------------------- *)

(* this module safe to open *)

open Base_types
open Prelude
open Block

(* block device ---------------------------------------- *)

(** Disk operations: read, write, and sync. FIXME move to int64 blkid *)
(*
type 't disk_opsFIXME = {
  blk_sz: blk_sz;
  read: blk_id -> (blk,'t) m;
  write: blk_id -> blk -> (unit,'t) m;
(*   disk_sync: unit -> (unit,'t) m; *)
}
*)

let wf_disk_ops 
    ~(blk_sz:blk_sz) 
    ~(read:blk_id -> (blk,'t) m) 
    ~(write:blk_id -> blk -> (unit,'t) m) 
  = true

let mk_disk_ops ~blk_sz ~read ~write =
  `Disk_ops(blk_sz,read,write)

let dest_disk_ops (`Disk_ops(blk_sz,read,write)) = 
  assert(wf_disk_ops ~blk_sz ~read ~write);
  fun k -> k ~blk_sz ~read ~write

let wf_imperative_disk_ops 
    ~(blk_sz:blk_sz)
    ~(read:blk_id -> blk)
    ~(write:blk_id -> blk -> unit)
  = true




let disk_ops_to_imperative ~blk_sz ~read ~write = 
  assert(wf_disk_ops ~blk_sz ~read ~write);
  fun ~_ref -> 
    let read r = 
      read r |> Monad.run !_ref |> fun (t',Ok blk) -> 
      _ref:=t';
      blk
    in
    let write r blk =
      write r blk |> Monad.run !_ref |> fun (t',Ok ()) ->
      _ref:=t';
      ()
    in
    assert(wf_imperative_disk_ops ~blk_sz ~read ~write);
    `Imperative_disk_ops(blk_sz,read,write)

(*
module Imperative_disk_ops = struct
  type idisk_ops = {
    blk_sz: blk_sz;
    read: blk_id -> blk;
    write: blk_id -> blk -> unit;
  }


  let of_disk_ops (ops:'t disk_ops) (_ref:'t ref) = (
    let read r = 
      ops.read r |> Monad.run !_ref |> fun (t',Ok blk) -> 
      _ref:=t';
      blk
    in
    let write r blk =
      ops.write r blk |> Monad.run !_ref |> fun (t',Ok ()) ->
      _ref:=t';
      ()
    in
    { blk_sz=ops.blk_sz; read; write}
  )
end
*)

(* store ------------------------------------------------------------ *)

(** Store operations: alloc, free and read. The store is a level above
   the raw disk, on which we build the B-tree. *)
(*
type ('k,'v,'r,'t) store_opsFIXME = ('k,'v,'r,'t) Store_ops.store_ops = {
  store_free: 'r list -> (unit,'t) m;
  store_read : 'r -> (('k, 'v,'r) frame,'t) m;  (* FIXME option? *)
  store_alloc : ('k, 'v,'r) frame -> ('r,'t) m;
}
*)

let wf_store_ops
    ~(store_free: 'r list -> (unit,'t) m)
    ~(store_read: 'r -> (('k, 'v,'r) frame,'t) m)  (* FIXME option? *)
    ~(store_alloc: ('k, 'v,'r) frame -> ('r,'t) m)
  =
  true



(* map ------------------------------------------------------------ *)

(* TODO insert_many *)

(** Map operations: find, insert, [insert_many] and
   delete. [insert_many] attempts to insert as many as possible in a
   single operation, and returns the remainder, and so is typically
   called in a loop. *)
(*
type ('k,'v,'t) map_opsFIXME = {
(*  find_leaf: 'k -> ( ('k*'v)list,'t) m; not in map api *)
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  insert_many: 'k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m;
  delete: 'k -> (unit,'t) m;
}
*)

(* NOTE insert_many not standard map op *)
let wf_map_ops
    ~(find: 'k -> ('v option,'t) m)
    ~(insert: 'k -> 'v -> (unit,'t) m)
    ~(delete: 'k -> (unit,'t) m)
    ~(insert_many: ('k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m) option)
  = 
  true


let mk_map_ops ~find ~insert ~delete ~insert_many =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  `Map_ops(find,insert,delete,insert_many)

let dest_map_ops (`Map_ops(find,insert,delete,insert_many)) =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  fun k -> k ~find ~insert ~delete ~insert_many
    
(** Call [insert_many] in a loop. *)
let rec insert_all insert_many k v kvs = Monad.(
    insert_many k v kvs |> bind (fun kvs' -> 
        match kvs' with
        | [] -> return ()
        | (k,v)::kvs -> insert_all insert_many k v kvs))


let wf_imperative_map_ops
    ~(find: 'k -> 'v option)
    ~(insert: 'k -> 'v -> unit)
    ~(delete: 'k -> unit)
  =
  true


let map_ops_to_imperative map_ops = 
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  assert(wf_map_ops ~find ~insert ~delete ~insert_many:None);
  fun ~s_ref ->
    let find k = 
      find k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let insert k v = 
      insert k v |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let delete k = 
      delete k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    assert(wf_imperative_map_ops ~find ~insert ~delete);
    `Imperative_map_ops(find,insert,delete)

let dest_imperative_map_ops (`Imperative_map_ops(find,insert,delete)) =
  assert(wf_imperative_map_ops ~find ~insert ~delete);
  fun k -> k ~find ~insert ~delete


(** Dummy module containing imperative version of the map interface. *)
(*
module Imperative_map_ops = struct

  (** Imperative version of the map interface; throws exceptions. WARNING likely to change. *)
  type ('k,'v) imap_ops = {
    find: 'k -> 'v option;
    insert: 'k -> 'v -> unit;
    delete: 'k -> unit;
  }

  open Monad
  let of_map_ops (map_ops:('k,'v,'t)map_ops) (s_ref:'t ref) = (
    let find k = 
      map_ops.find k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let insert k v = 
      map_ops.insert k v |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let delete k = 
      map_ops.delete k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    {find;insert;delete})

  (* FIXME make sure other places where we expect OK actually do
     something with the error *)

end
*)

(* leaf stream ------------------------------------------------------ *)


(* we only reveal lss when it points to a leaf *)

(** Leaf stream representation. This type is for debugging - you
   shouldn't need to access components. *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)Small_step.ls_state }

(** A leaf stream is a linear sequence of the leaves in a B-tree, used
   for iterating over all the bindings in the tree. Leaf stream
   operations: make a leaf stream; get the list of (key,value) pairs
   associated to the state of the leaf stream; step to the next
   leaf. *)
(*
type ('k,'v,'r,'t) ls_ops = {
  mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m;
  ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m;
  ls_kvs: ('k,'v,'r) lss -> ('k*'v) list
}
*)

let wf_ls_ops 
    ~(mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m)
    ~(ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m)
    ~(ls_kvs: ('k,'v,'r) lss -> ('k*'v) list)
  =
  true

let mk_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs =
  assert(wf_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs);
  `Ls_ops(mk_leaf_stream,ls_step,ls_kvs)

let dest_ls_ops (`Ls_ops(mk_leaf_stream,ls_step,ls_kvs)) = 
  assert(wf_ls_ops ~mk_leaf_stream ~ls_step ~ls_kvs);
  fun k -> k ~mk_leaf_stream ~ls_step ~ls_kvs


(* for debugging *)

(** Get all (key,value) pairs from a leaf stream. Debugging only. *)
let all_kvs ~ls_ops : (('k * 'v) list,'t) m = Monad.(
    dest_ls_ops ls_ops @@ fun ~mk_leaf_stream ~ls_step ~ls_kvs ->
    let rec loop kvs s = (
      let kvs' = ls_kvs s in
      let kvs = kvs@kvs' in
      ls_step s |> bind (fun s' ->
          match s' with
          | None -> return kvs
          | Some s' -> loop kvs s'))
    in
    mk_leaf_stream () |> bind (fun s -> loop [] s))


