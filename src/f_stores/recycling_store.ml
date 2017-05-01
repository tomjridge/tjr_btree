(* a recycling store on top of a normal store *)

(* TODO implement a sound version of leaf stream operations given a recycling store *)

(* cache (page_ref -> frame), and if these refs freed without being
   synced, we recycle them *)

(* a store that attempts to recycle blocks that will never end up on
   disk; we maintain a set of blocks that have been allocated and not
   freed since last sync (ie which need to be written), and a set of
   page refs that have been allocated since last sync and freed
   without being synced (ie which don't need to go to store at all) *)

(* FIXME worth checking no alloc/free misuse? *)

(* FIXME should use the LRU cache *)


open Prelude
open Btree_api
open Page_ref_int
open Simple_monad 

module O = struct
  (* need to cache page_ref to ('k,'v)frame *)
  module Map_page_ref = Map_int
  module Cache = Map_page_ref  (* Cache.t ~ (page_ref -> frame) *)
  module FNS = Set_int (* free not synced page_refs *)

  type ('k,'v) cache = ('k,'v) frame Cache.t

  type fns = FNS.t 

(*
Recycling state:

- a cache of pages which need to be written

- fns, the "freed-not-synced" page_refs; really this is "don't write
  to store on sync"

FIXME don't we also need to know which were allocated since last sync?
*) 
  type ('k,'v) recycling_state = {
    cache: ('k,'v) cache; 
    fns: fns 
  }

  (* operations from lower store; allocate without writing *)
  type ('k,'v,'r,'t) lower_ops = {
    store_ops: ('k,'v,'r,'t) store_ops;
    store_alloc_page_ref : unit -> ('r,'t) m;
    store_write_frame: 'r -> ('k,'v) frame -> (unit,'t) m;
  }

  (* we provide extra operations store_sync which flushes to lower *)
  type ('k,'v,'r,'t) rs_ops = {
    store_ops: ('k,'v,'r,'t) store_ops; 
    store_sync: unit -> (unit,'t) m;
  }

  type ('k,'v,'t) rs_params = {
    get_rs: unit -> (('k,'v) recycling_state,'t) m;
    set_rs: ('k,'v) recycling_state -> (unit,'t) m
  }
end

open O

let make lower ps : ('k,'v,'r,'t) rs_ops = (
  (* cache functions *)
  let get_cache () : (('k,'v) cache,'t) m = 
      ps.get_rs() |> bind (fun s -> return s.cache)
  in
  let set_cache c : (unit,'t) m = 
      ps.get_rs() |> bind (fun s -> ps.set_rs {s with cache=c})
  in
  let clear_cache: unit -> (unit,'t) m = (fun () -> set_cache Cache.empty) in
  let cache_add: 'r -> ('k,'v,'r)Frame.frame -> (unit,'t) m = (fun r p ->
      get_cache () |> bind (fun c ->
          set_cache (Cache.add r p c)))
  in
  (* fns functions *)
  let get_fns: unit -> (fns,'t) m = (fun () ->
      ps.get_rs() |> bind (fun s -> return s.fns))
  in
  let set_fns: fns -> (unit,'t) m = (fun fns ->
      ps.get_rs() |> bind (fun s -> ps.set_rs {s with fns=fns}))
  in
  let get_1_fns: unit -> ('r option,'t) m = (fun () -> 
      get_fns () |> bind (fun fns -> 
          match (FNS.is_empty fns) with
          | true -> return None
          | false -> 
            fns
            |> FNS.min_elt 
            |> (fun r -> return (Some r))))
  in
  let fns_remove: 'r -> (unit,'t) m = (fun r -> 
      get_fns () |> bind (fun fns -> 
          set_fns (FNS.remove r fns)))
  in
  let store_free: 'r list -> (unit,'t) m = (fun rs ->
      get_fns () |> bind (fun fns ->
          set_fns (FNS.union fns (FNS.of_list rs))))
  in
  let store_alloc: ('k,'v,'r)Frame.frame -> ('r,'t) m = (fun p ->
      get_1_fns () |> bind (fun r -> 
          match r with 
          | None -> (
              (* we need the lower store to be able to alloc without writing *)
              lower.store_alloc_page_ref () |> bind (fun r ->
                  cache_add r p |> bind (fun () -> 
                      return r)))
          | Some r -> (
              (* just return a ref we allocated previously *)
              fns_remove r |> bind (fun () -> 
                  cache_add r p |> bind (fun () -> 
                      return r))) ))
  in
  let store_read: 'r -> (('k,'v) frame,'t) m = (fun r ->
      (* check cache, otherwise default to lower *)
      get_cache () |> bind (fun c ->
          (try Some(Cache.find r c) with Not_found -> None)
          |> (function 
              | Some p -> return p
              | None -> lower.store_ops.store_read r)))
  in
  (* FIXME on a sync, freed_not_synced needs to be updated? at least,
     it isn't quite right at the moment ? *)
  let store_sync: unit -> (unit,'t) m = (fun () ->
      get_cache () |> bind (fun cache -> 
          let es = Cache.bindings cache in
          get_fns () |> bind (fun fns -> 
              let rec loop es = (
                match es with 
                | [] -> (return ())
                | (r,p)::es -> (
                    match (FNS.mem r fns) with 
                    | true -> loop es (* don't sync if freed *)
                    | false -> (
                        (* lower_write_frame needed here *)
                        lower.store_write_frame r p |> bind (fun () ->
                            loop es))))
              in
              loop es |> bind (fun () -> 
                  clear_cache () |> bind (fun () -> 
                      return () 
                      (* NB syncing of lower levels done
                         manually lower.store_sync () *)
                    )))))
  in
  let store_ops = { store_free; store_read; store_alloc } in
  { store_ops; store_sync })





(*
  (* FIXME can't this be derived from store_read? why needed in interface? *)
  let mk_r2f: W.t -> page_ref -> ('k,'v)frame option = (fun t ->
      let lower_r2f = lower.store_ops.mk_r2f t in
      fun r -> (
          let m = (
            get_cache () |> bind (fun c -> 
                (* consult cache first *)
                (try Some(Cache.find r c) with Not_found -> None)
                |> (function
                    | Some p -> (return (Some p))
                    | None -> (
                        return (lower_r2f r)))))
          in
          match (m t) with
          | (_,Ok f) -> f
          | (_,Error e) -> (
              (* assume only called on page_refs that are valid *)
              ignore(assert(false)); None)))  
  in
*)
