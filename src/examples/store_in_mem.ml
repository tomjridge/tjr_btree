(** Simple in-memory store implementation, mainly for testing *)
module Blk_id = Blk_id_as_int
open Blk_id

type page_ref = blk_id[@@deriving bin_io]

(** In-mem store, a map from r (int) to [('k,'v)dnode_impl] *)
type 'dnode mem = {free:int; map:'dnode Map_int.t}  

type ('dnode,'t) mem_ops = ('dnode mem,'t) with_state

let mk_store_ops ~monad_ops ~(mem_ops:('dnode,'t)mem_ops) =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { with_state } = mem_ops in
  let read r : ('dnode,'t) m =
    with_state (fun ~state:s ~set_state:_ ->
        return (Map_int.find (Blk_id.to_int r) s.map)) (* ASSUMES present *)
  in
  let wrte dn : (page_ref,'t) m = 
    with_state (fun ~state:s ~set_state ->
        let s' = {free=s.free+1; map=Map_int.add s.free dn s.map} in
        set_state s' >>= fun () -> 
        return (Blk_id.of_int s.free))
  in
  let rewrite r dn : (page_ref option,'t) m = 
    (* always rewrite *)
    with_state (fun ~state:s ~set_state ->
        let s' = {s with map=Map_int.add (Blk_id.to_int r) dn s.map} in
        set_state s' >>= fun () -> 
        return None)
  in    
  let free _rs : (unit,'t) m = return () in (* no-op *)
  { read; wrte; rewrite; free }

let _ = mk_store_ops


(* debugging -------------------------------------------------------- *)

(*
let mk_r2f get_store s r : ('k,'v,int)dode option = 
  get_store s |> fun mem -> 
  try Some (Map_int.find r mem.map)
  with Not_found -> None
*)

(* param FIXME move *)
(* let mem_ops x : ('k,'v,'t) mem_ops = x#mem_ops *)

