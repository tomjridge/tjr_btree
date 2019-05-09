(** Simple in-memory store implementation, mainly for testing *)
(* open Tjr_monad *)
open Tjr_monad.Types
open Tjr_monad.Mref
open Isa_btree.Isa_export_wrapper
open Tjr_btree
open Page_ref_int

module Map_int = Tjr_map.Map_int

(** In-mem store, a map from r (int) to [('k,'v)dnode_impl] *)
type ('k,'v) mem = {free:int; map:('k,'v,int)dnode_impl Map_int.t}  

type ('k,'v,'t) mem_ops = (('k,'v) mem,'t) mref

let mk_store_ops ~monad_ops ~(mem_ops:('k,'v,'t)mem_ops) =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let read r : (('k,'v,int)dnode_impl,'t) m =
    mem_ops.get () >>= fun s ->
    return (Map_int.find r s.map) (* ASSUMES present *)
  in
  let wrte dn : (page_ref,'t) m = 
    mem_ops.get () >>= fun s -> 
    let s' = {free=s.free+1; map=Map_int.add s.free dn s.map} in
    mem_ops.set s' >>= fun () -> 
    return s.free
  in
  let rewrite r dn : (int option,'t) m = 
    (* always rewrite *)
    mem_ops.get () >>= fun s -> 
    let s' = {s with map=Map_int.add r dn s.map} in
    mem_ops.set s' >>= fun () -> 
    return None
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

