(** Simple in-memory store implementation, mainly for testing *)

open Base_types
open Page_ref_int


(* in mem store *)
type ('k,'v) mem = {free:int; map:('k,'v)frame Map_int.t}  

type ('k,'v,'t) mem_ops = (('k,'v) mem,'t) mref

let mk_store_ops ~monad_ops ~(mem_ops:('k,'v,'t)mem_ops) =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let store_free _rs : (unit,'t) m = return () in (* no-op *)
  let store_alloc f : (page_ref,'t) m = 
    mem_ops.get () >>= fun s -> 
    let s' = {free=s.free+1; map=Map_int.add s.free f s.map} in
    mem_ops.set s' >>= fun () -> 
    return s.free
  in
  let store_read r : (('k,'v) frame,'t) m =
    mem_ops.get () >>= fun s ->
    return (Map_int.find r s.map) (* ASSUMES present *)
  in
  Store_ops.mk_store_ops ~store_free ~store_read ~store_alloc

let _ = mk_store_ops

let mk_r2f get_store s r : ('k,'v)frame option = 
  get_store s |> fun mem -> 
  try Some (Map_int.find r mem.map)
  with Not_found -> None


(* param FIXME move *)
let mem_ops x = x#mem_ops

