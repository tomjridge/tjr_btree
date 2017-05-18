(** A map from blkid to blk, implemented as a map from blkid to blkid *)

open Base_types
open Prelude
open Btree_api
open Default
open Monad

type blkid = BLK.r
type blk = BLK.t


(* as usual, we implement on top of a store *)

let store_ops_to_map_ops ps pr_ops store_ops : (blkid,blkid,'t) map_ops = (
  let compare_k=(BLK.compare_r) in
  let constants=(constants ps) in
  let debug=None in
  let ps = object
    method compare_k=compare_k
    method constants=constants
    method debug=debug
  end in
  Store_to_map.store_ops_to_map_ops ps pr_ops store_ops
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
          | None -> err __LOC__  (* TODO? *)
          | Some blkid -> read_blk blkid))
  in
  let insert : 'k -> 'v -> (unit,'t) m = (fun i blk ->
      (* allocate a new blk from disk *)
      write_blk blk |> bind (
        fun blkid -> 
          (* insert k,blkid into btree *)
          map_ops.insert i blkid))
  in
  let delete : 'k -> (unit,'t) m = (fun i -> 
      (* no-op: we never "delete" a particular block *)
      failwith __LOC__)
  in
  {find; insert; delete }
)

