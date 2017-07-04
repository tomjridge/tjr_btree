(** A map from blk index to blk, implemented as a map from index to blkid *)

open Base_types
open Prelude
open Btree_api
open Monad
open Params
open Block


(* as usual, we implement on top of a store *)

type k1 = int  (* index *)
type v1 = blk_id  (* location of underlying block that corresponds to this index *)

let store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops ~kk = (
  let cmp=(Int_.compare) in
  let dbg_ps=None in
  let ps = object
    method cmp=cmp
    method constants=(constants ps)
    method dbg_ps=None
  end in
  Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops
    ~kk:(fun ~(map_ops:(k1,v1,'t)map_ops) ~find_leaf -> kk ~map_ops ~find_leaf)
)

(* the map index->blk_id (k1->v1) is then used to implement a map index->blk *)

type k2 = k1
type v2 = blk

let mk_int_blk_map 
    ~(write_blk:blk->(blk_id,'t)m)  (* write blk in data *)
    ~(read_blk:blk_id->(blk option,'t)m)
    ~(map_ops:(k1,v1,'t)map_ops) 
  : (k2,v2,'t) map_ops = (
  let find : 'k -> ('v option,'t) m = (fun i ->
      (* read from map *)
      map_ops.find i |> bind (
        fun blk_id -> 
          match blk_id with
          | None -> return None  (* FIXME or empty block? *)
          | Some blk_id -> read_blk blk_id))
  in
  let insert : 'k -> 'v -> (unit,'t) m = (fun i blk ->
      (* allocate a new blk from disk *)
      write_blk blk |> bind (
        fun blk_id -> 
          (* insert k,blk_id into btree *)
          map_ops.insert i blk_id))
  in
  (* NOTE following returns an empty list, since we really want to
     insert all the blocks *)
  let insert_many: 'k -> 'v -> ('k*'v)list -> (('k*'v)list,'t) m = (
    fun k v kvs ->
      (* allocate lots of new blks from disk *)
      let rec loop (_done,todo) = (
        match todo with
        | [] -> return _done
        | (blk_id,blk)::todo' -> (
            write_blk blk |> bind (fun blk_id' ->
                loop( (blk_id,blk_id')::_done,todo'))))
      in
      loop ([],(k,v)::kvs) |> bind (fun xs -> 
          match xs with
          | [] -> impossible __LOC__
          | (i,i')::xs -> insert_all map_ops.insert_many i i' xs |> bind (
              fun () -> return [])))
  in
  let delete : 'k -> (unit,'t) m = (fun i -> 
      (* no-op: we never "delete" a particular block *)
      failwith __LOC__)
  in
  {find; insert; insert_many; delete }
)

