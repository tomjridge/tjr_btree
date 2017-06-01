(** A map from blkid to blk, implemented as a map from blkid to blkid *)

open Base_types
open Prelude
open Btree_api
open Default
open Monad
open Params

type blkid = BLK.r
type blk = BLK.t


(* as usual, we implement on top of a store *)

let store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops : (blkid,blkid,'t) map_ops = (
  let cmp=(BLK.compare_r) in
  let dbg_ps=None in
  let ps = object
    method cmp=cmp
    method constants=(constants ps)
    method dbg_ps=None
  end in
  Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops
)

(* the map blkid->blkid is then used to implement a map blkid->blk,
   which in turn is used to implement a snapshottable disk interface! *)

type k = blkid
type v = blk

let mk_blkid_blk_map 
    (write_blk:blk->(blkid,'t)m)  (* write blk in data *)
    (read_blk:blkid->(blk option,'t)m)
    (map_ops:(blkid,blkid,'t)map_ops) 
  : (k,v,'t) map_ops = (
  let find : 'k -> ('v option,'t) m = (fun i ->
      (* read from map *)
      map_ops.find i |> bind (
        fun blkid -> 
          match blkid with
          | None -> return None  (* FIXME or empty block? *)
          | Some blkid -> read_blk blkid))
  in
  let insert : 'k -> 'v -> (unit,'t) m = (fun i blk ->
      (* allocate a new blk from disk *)
      write_blk blk |> bind (
        fun blkid -> 
          (* insert k,blkid into btree *)
          map_ops.insert i blkid))
  in
  (* NOTE following returns an empty list, since we really want to
     insert all the blocks *)
  let insert_many: 'k -> 'v -> ('k*'v)list -> (('k*'v)list,'t) m = (
    fun k v kvs ->
      (* allocate lots of new blks from disk *)
      let rec loop (_done,todo) = (
        match todo with
        | [] -> return _done
        | (blkid,blk)::todo' -> (
            write_blk blk |> bind (fun blkid' ->
                loop( (blkid,blkid')::_done,todo'))))
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

