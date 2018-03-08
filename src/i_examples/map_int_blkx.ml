(** Map from int to partial blk, implemented as a map from blkid to blkid*sz *)

(* used by bytestore2 *)

open Base_types
open Monad
open Params
open Block
open Map_ops

(* as usual, we implement on top of a store *)

(* v is something like a blk_id * sz; implement a map where value is blk * sz *)
module Mk = functor (
  X:sig 
    type v 
    val v2blk_id: v -> blk_id 
    val mk_v: blk_id:int -> sz:int -> v
  end) -> struct

  open X

  let store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops : ('k,'v,'t) map_ops = 
    let cmp=(Block.compare_blk_id) in
    let dbg_ps=None in
    let ps = object
      method cmp=cmp
      method constants=(constants ps)
      method dbg_ps=None
    end in
    Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops


  (* the map blk_id->blk_id is then used to implement a map blk_id->blk,
     which in turn is used to implement a snapshottable disk interface! *)

  type k = blk_id

  let mk_blk_id_blk_map 
      ~(write_blk:blk->(blk_id,'t)m)  (* write blk in data *)
      ~(read_blk:blk_id->(blk option,'t)m)
      ~map_ops  (* (blk_id,v,'t)map_ops *)
    = 
    dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
    let find : 'k -> ('v option,'t) m = fun i ->
      (* read from map *)
      find i |> bind (
        fun v -> 
          match v with
          | None -> return None 
          | Some v -> v|>v2blk_id|>read_blk)
    in
    let insert : 'k -> blk*int -> (unit,'t) m = fun i v ->
      let (blk,sz) = v in
      (* allocate a new blk from disk *)
      write_blk blk |> bind (
        fun blk_id -> 
          (* insert k,blk_id into btree *)
          insert i (mk_v ~blk_id ~sz))
    in
    (* NOTE following returns an empty list, since we really want to
       insert all the blocks *)
    let insert_many =  None (* TODO *) in
    let delete : 'k -> (unit,'t) m = (fun i -> 
        (* no-op: we never "delete" a particular block TODO truncate? *)
        failwith __LOC__)
    in
    fun k -> k  ~find ~insert

end
