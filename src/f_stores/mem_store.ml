(** Simple in-memory store implementation, mainly for testing *)

open Page_ref_int
open Monad


(* in mem store *)
type ('k,'v) mem = {free:int; map:('k,'v)frame Map_int.t}  

type ('k,'v,'t) mem_ops = (('k,'v) mem,'t) mref

let mk_store_ops (mem_ops:('k,'v,'t)mem_ops) =
  let store_free rs : (unit,'t) m = return () in (* no-op *)
  let store_alloc f : (page_ref,'t) m = 
    mem_ops.get () |> bind @@ fun s -> 
    let s' = {free=s.free+1; map=Map_int.add s.free f s.map} in
    mem_ops.set s' |> bind @@ fun () -> 
    return s.free
  in
  let store_read r : (('k,'v) frame,'t) m =
    mem_ops.get () |> bind @@ fun s ->
    return (Map_int.find r s.map) (* ASSUMES present *)
  in
  Store_ops.mk_store_ops ~store_free ~store_read ~store_alloc

let mk_r2f get_store s r : ('k,'v)frame option = 
  get_store s |> fun mem -> 
  try Some (Map_int.find r mem.map)
  with Not_found -> None


(* param *)
let mem_ops x = x#mem_ops

