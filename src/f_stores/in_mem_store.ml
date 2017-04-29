(* simple in-mem implementation, mainly for testing ----------------- *)


open Prelude
open Btree_api
open Page_ref_int

(* in mem store *)
type ('k,'v) im = {free:int; map:('k,'v)frame Map_int.t}  

type ('k,'v,'t) in_mem_ops = {
  get_store: unit -> (('k,'v) im,'t) m;
  set_store: ('k,'v) im -> (unit,'t) m;     
}

open Simple_monad

let make (im_ops:('k,'v,'t)in_mem_ops) : ('k,'v,'r,'t) store_ops = (
  let store_free rs : (unit,'t) m = return () in (* no-op *)
  let store_alloc f : (page_ref,'t) m = 
    im_ops.get_store () |> bind (fun s -> 
        let s' = {free=s.free+1; map=Map_int.add s.free f s.map} in
        im_ops.set_store s' |> bind (fun () -> 
            return s.free))
  in
  let store_read r : (('k,'v) frame,'t) m =
    im_ops.get_store () |> bind (fun s ->
        return (Map_int.find r s.map)) (* ASSUMES present *)
  in
  { store_free; store_read; store_alloc }
)

let mk_r2f get_store s r : ('k,'v)frame option = (
  s|>get_store|>(fun im -> 
      try Some (Map_int.find r im.map)
      with Not_found -> None))

  

(*let store_sync: t -> unit m = (fun t -> return ())  (* no-op *) *)



(* old ============================================================ *)

(* for yojson *)
                                                 (*
      type store' = {free':int; m':(int * Page.t) list}[@@deriving yojson]

      let store_to_' s = {free'=s.free; m'=s.m|>Map_int.bindings}
                                                  *)
      (*
      let dest_Store : store -> page_ref -> page = (
        fun s r -> Map_int.find r s.m)
*)
