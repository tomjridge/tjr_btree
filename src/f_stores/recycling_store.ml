(* a recycling store on top of a normal store *)

(* a store that attempts to recycle blocks that will never end up on
   disk *)

(* a filestore which caches page writes and recycles page refs *)

(* we maintain a set of blocks that have been allocated and not freed
   since last sync (ie which need to be written), and a set of page
   refs that have been allocated since last sync and freed without
   being synced (ie which don't need to go to store at all) *)

(* FIXME worth checking no alloc/free misuse? *)

(* FIXME should use the LRU cache *)

module type LOWER = sig  (* lower store *)
  include STORE with type Page.r = int and type 'a m = 'a World.m
  val alloc_block: t -> Page.r m
  val write: t -> Page.r -> Page.t -> unit m
end

module Recycling_store = functor (Lower:LOWER) -> struct
  module R_ = (struct 
    module Lower = Lower (* lower store *)
    module Page = Lower.Page

    module Cache = Btree_util.Map_int
    module FNS = Btree_util.Set_int (* free not synced *)

    type cache = Page.t Cache.t
    type fns = FNS.t  
    type store = {
      lower: Lower.t;  (* underlying store *)
      cache: cache;  (* a cache of pages which need to be written *)
      fns: fns  (* really this is "don't write to store on sync" *)
      (* FIXME don't we also need to know which were allocated since last sync? *)
    }

    open World
    type t = store World.r
    type 'a m = 'a World.m
    let bind = World.bind
    let return = World.return

    (* store.lower ---------- *)

    let get_lower: t -> Lower.t m = (fun t ->
        get t |> bind (fun s -> return s.lower))

    (* store.cache ---------- *)

    let get_cache: t -> cache m = (fun t ->
        get t |> bind (fun s -> return s.cache))

    let clear_cache: t -> unit m = (fun t -> 
        get t |> bind (fun s -> 
            set t {s with cache=Cache.empty}))

    let cache_add: t -> Page.r -> Page.t -> unit m = (fun t r p ->
        get t |> bind (fun s ->
            set t {s with cache=Cache.add r p s.cache}))

    let free: t -> Page.r list -> unit m = (fun t rs ->
        get t |> bind (fun s ->
            set t {s with fns=FNS.union s.fns (FNS.of_list rs)}))


    (* store.fns ---------- *)

    let get_fns: t -> fns m = (fun t ->
        get t |> bind (fun s -> return s.fns))

    let get_1_fns: t -> Page.r option m = (fun t -> 
        get_fns t |> bind 
          (fun fns -> 
             match (FNS.is_empty fns) with
             | true -> return None
             | false -> 
               fns
               |> FNS.min_elt 
               |> (fun r -> return (Some r))))

    let fns_remove: t -> Page.r -> unit m = (
      fun t r -> 
        get t |> bind (fun s -> 
            set t {s with fns=(FNS.remove r s.fns)}))



    let alloc: t -> Page.t -> Page.r m = (fun t p ->
        get_1_fns t |> bind
          (fun r -> 
             match r with 
             | None -> (
                 get_lower t |> bind (fun lower ->
                     Lower.alloc_block lower |> bind (fun r ->
                         cache_add t r p |> bind (fun () -> 
                             return r))))
             | Some r -> (
                 (* just return a ref we allocated previously *)
                 fns_remove t r |> bind (fun () -> 
                     cache_add t r p |> bind (fun () -> 
                         return r)))))

    let page_ref_to_page: t -> Page.r -> Page.t m = (fun t r ->
        get_cache t |> bind (fun cache -> 
            (* consult cache first *)
            (try Some(Cache.find r cache) with Not_found -> None)
            |> (function
                | Some p -> (return p)
                | None -> (
                    get_lower t |> bind (fun lower ->
                        Lower.page_ref_to_page lower r)))))


    (* FIXME on a sync, freed_not_synced needs to be updated? at least,
       it isn't quite right at the moment *)
    let store_sync: t -> unit m = (fun t ->
        get_lower t |> bind (fun lower -> 
            get_cache t |> bind (fun cache -> 
                let es = Cache.bindings cache in
                get_fns t |> bind (fun fns -> 
                    let rec loop es = (
                      match es with 
                      | [] -> (return ())
                      | (r,p)::es -> (
                          match (FNS.mem r fns) with 
                          | true -> loop es (* don't sync if freed *)
                          | false -> (
                              (* to avoid writes, we need to have access
                                 to the write function from the lower
                                 store *)
                              Lower.write lower r  p |> bind (fun () ->
                                  loop es))))
                    in
                    loop es |> bind (fun () -> 
                        clear_cache t |> bind (fun () -> 
                            Lower.store_sync lower))))))

  end) (* R_ *)

  let _ = (module R_ : STORE)

  include R_
end



