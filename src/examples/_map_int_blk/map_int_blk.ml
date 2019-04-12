(** A map from blk index to blk, implemented as a map from index to blkid *)

open Base_types
(* open Params *)
(* open Block *)
open Map_ops

(* as usual, we implement on top of a store *)

type k1 = int  (* index *)

(* location of underlying block that corresponds to this index *)
type blk_id = int
type v1 = blk_id  

let store_ops_to_map_ops 
    ~monad_ops ~constants ~page_ref_ops ~store_ops : ('k,'v,'t) map_ops 
  =
  Store_to_map.store_ops_to_map_ops 
    ~monad_ops ~constants ~cmp:Tjr_int.compare ~page_ref_ops ~store_ops 

(* the map index->blk_id (k1->v1) is then used to implement a map index->blk *)

type k2 = k1
(* type v2 = blk *)

let mk_int_blk_map 
    ~monad_ops
    ~(write_blk:'blk->(blk_id,'t)m)  (* write blk in data *)
    ~(read_blk:blk_id->('blk option,'t)m)
    ~map_ops
  : ('k,'v,'t) map_ops 
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { find; insert; insert_many; _ } = map_ops in
  let find : 'k -> ('v option,'t) m = (fun i ->
      (* read from map *)
      find i >>= fun blk_id -> 
      match blk_id with
      | None -> return None  (* FIXME or empty block? *)
      | Some blk_id -> read_blk blk_id)
  in
  let insert : 'k -> 'v -> (unit,'t) m = (fun i blk ->
      (* allocate a new blk from disk *)
      write_blk blk >>= fun blk_id -> 
      (* insert k,blk_id into btree *)
      insert i blk_id)
  in
  (* NOTE following returns an empty list, since we really want to
     insert all the blocks *)
  let insert_all = insert_all ~monad_ops in
  let insert_many: 'k -> 'v -> ('k*'v)list -> (('k*'v)list,'t) m =
    fun k v kvs ->
      (* allocate lots of new blks from disk *)
      let rec loop (_done,todo) = (
        match todo with
        | [] -> return _done
        | (blk_id,blk)::todo' -> (
            write_blk blk >>= (fun blk_id' ->
                loop( (blk_id,blk_id')::_done,todo'))))
      in
      loop ([],(k,v)::kvs) >>= (fun xs -> 
          match xs with
          | [] -> impossible __LOC__
          | (i,i')::xs -> insert_all insert_many i i' xs >>= (
              fun () -> return []))
  in
  let delete : 'k -> (unit,'t) m = (fun _i -> 
      (* no-op: we never "delete" a particular block FIXME use
         different type for map_ops? or just pass on functions directly? *)
      failwith __LOC__)
  in
  { find; insert; delete; insert_many }
