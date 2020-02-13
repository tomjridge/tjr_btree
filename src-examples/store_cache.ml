(** Add LRU caching to an existing store. This is a read cache; writes
   are passed through in addition to being cached. So there is no need
   to flush anything. *)


(* FIXME move elsewhere eg fs_shared *)

open Profilers_.Lru_profiler

(** NOTE this adds an imperative read cache; take care with testing etc *)
let add_imperative_read_cache_to_store (* make_store_with_lru *) (type blk_id node leaf) ~monad_ops ~store_ops =
  let module A = struct
    let ( >>= ) = monad_ops.bind 
    let return = monad_ops.return

    let [m1;m2;m3] = 
      ["m1";"m2";"m3"] |> List.map intern
    [@@ocaml.warning "-8"]

    (* FIXME possibly inefficient *)
    let profile_m = 
      fun s m ->    
      return () >>= fun () -> 
      mark s;
      m >>= fun r ->
      mark (-1*s);
      return r

    (* we add some profiling; we also take the opportunity to add some
       simple caching; FIXME add LRU caching for store *)
    let store_ops = 
      let {read;wrte;rewrite;free} = store_ops in
      (* Add some memoization *)
      let module L = Lru.M.Make(struct
          type t = blk_id
          let equal : t -> t -> bool = Pervasives.(=)  (* FIXME don't use pervasives for real code *)
          let hash: t -> int = Hashtbl.hash
        end)(struct
          type t = (node, leaf) dnode  (* FIXME dnode_impl *)
          let weight: t -> int = fun _ -> 1
        end)
      in    
      let cap = 52000 (* 51539*) in   (* FIXME config; (+ 10 ( * 227 227)) *)
      let slack = 10 in
      let lru = L.create cap in
      (* FIXME we perhaps want to avoid calling trim on every add
         (depending on the cost of performing a trim) *)
      let trim lru = 
        if L.size lru >= cap+slack then L.trim lru else ()
      in
      let read = fun r -> 
        L.find r lru |> function
        | None -> (read r >>= fun dn -> 
                   L.add r dn lru; 
                   trim lru;
                   ();
                   return dn)
        | Some dn -> 
          L.promote r lru;
          return dn
      in
      let wrte dn = 
        wrte dn >>= fun r -> 
        L.add r dn lru;
        trim lru;
        return r
      in
      let rewrite r dn = 
        rewrite r dn >>= function
        | None -> (
            (* updated in place *)
            L.add r dn lru;
            trim lru;
            return None)
        | Some r' -> (
            L.add r' dn lru;
            trim lru;
            return (Some r'))
      in
      {
        read=(fun r -> profile_m m1 (read r));
        wrte=(fun dn -> profile_m m2 (wrte dn));
        rewrite=(fun r dn -> profile_m m3 (rewrite r dn));
        free;
      }
    
  end 
  in
  A.store_ops


(** NOTE alloc is necessary because a write of a node may only allocate the block, rather than actually writing.

NOTE that eviction should be handled asynchronously, by repeatedly calling wrte on the lower store_ops
 *)
let add_write_back_cache_to_store (type blk_id)
      ~monad_ops ~store_ops 
      ~alloc 
      ~(evict:(blk_id * 'v)list -> (unit,'t)m)
      ~(write_back_cache_ops:(blk_id,'v,_,'wb) Write_back_cache.write_back_cache_ops)
      ~with_write_back_cache 
  = 
  let module A = struct
    let ( >>= ) = monad_ops.bind 
    let return = monad_ops.return

    let [read_;wrte_;rewrite_] = 
      ["read";"wrte";"rewrite"] |> List.map intern
    [@@ocaml.warning "-8"]

    (* FIXME possibly inefficient *)
    let profile_m = 
      fun s m ->    
      return () >>= fun () -> 
      mark s;
      m >>= fun r ->
      mark (-1*s);
      return r

    (* we add some profiling; we also take the opportunity to add some
       simple caching; FIXME add LRU caching for store *)
    let store_ops = 
      let {read;wrte=_NOTE_NOT_USED;rewrite=_NOTE_NOT_USED';free} = store_ops in
      (* Add some memoization *)
      let module L = Write_back_cache.Make_write_back_cache(struct
                       type t = blk_id
                       let compare : t -> t -> int = Pervasives.compare  (* FIXME don't use pervasives for real code *)
                     end)
      in
      let wb = write_back_cache_ops in
      (* lift trim_if_over_cap to monad FIXME rename to trim_and_set_cache *)
      let trim_and_set_cache ~set_state cache = 
        cache |> wb.trim_if_over_cap |> function
        | None -> set_state cache
        | Some(evictees,cache') -> (
            evict evictees >>= fun () -> 
            set_state cache')
      in
      let read = fun r -> 
        with_write_back_cache.with_state (fun ~state:cache ~set_state -> 
          wb.find r cache |> function
          | None -> (
              (* read and insert a clean entry into the cache *)
              read r >>= fun dn -> 
              wb.insert r (dn,false) cache |> fun cache' -> 
              trim_and_set_cache ~set_state cache' >>= fun () -> return dn)
          | Some (dn,_dirty) -> (
              wb.promote r cache |> fun cache' -> 
              set_state cache' >>= fun () -> return dn)
        )         
      in
      (* for write, we allocate a block, but don't write immediately;
         attempts to rewrite will succeed automatically (?); any
         checkpoint operations must presumably ensure that all data is
         on disk (ie caches are empty) *)
      let wrte dn = 
        alloc () >>= fun r -> 
        with_write_back_cache.with_state (fun ~state:cache ~set_state ->
          wb.insert r (dn,true) cache |> fun cache' ->
          trim_and_set_cache ~set_state cache') >>= fun () ->         
        return r
      in
      (* for rewrite 
         (if we have a cached entry
           (cond 
             ((entry is dirty) (we can rewrite; this is the case if we just wrote the entry))
             ((otherwise entry is clean) (we could either try to lower.rewrite which is lengthy, or just assume that we can't rewrite)))
         else 
           (nothing in cache)
           (again, we can't be sure what would happen in the future, so we probably assume that we can't rewrite)
      *)         
      let rewrite r dn = 
        with_write_back_cache.with_state (fun ~state:cache ~set_state -> 
          let alloc_and_place_dirty_entry_in_cache () = 
            alloc () >>= fun r -> 
            wb.insert r (dn,true) cache |> fun cache' -> 
            trim_and_set_cache ~set_state cache' >>= fun () -> return (Some r)
          in
          wb.find r cache |> function
          | None -> (
              (* assume we can't rewrite *)
              alloc_and_place_dirty_entry_in_cache ())
          | Some (_dn,dirty) -> (  (* was dn rather than _dn! nasty bug *)
              match dirty with
              | true -> (
                  wb.insert r (dn,true) cache |> fun cache' -> 
                  set_state cache' >>= fun () -> return None)
              | false -> (
                  alloc_and_place_dirty_entry_in_cache ()))
        )
      in
      let free blk_ids = 
        print_endline "Call to free";
        ignore(failwith __LOC__); (* FIXME remove *)
        (* these blocks are guaranteed to never be accessed again; so we can remove them from the cache *)
        with_write_back_cache.with_state (fun ~state:cache ~set_state -> 
          let cache = 
            (cache,blk_ids) |> iter_k (fun ~k (cache,blk_ids) -> 
                match blk_ids with
                | [] -> cache
                | x::xs -> 
                  wb.delete x cache |> fun cache' ->
                  k (cache',xs))
          in
          set_state cache) >>= fun () -> 
        free blk_ids
      in
      {
        read=(fun r -> profile_m read_ (read r));
        wrte=(fun dn -> profile_m wrte_ (wrte dn));
        rewrite=(fun r dn -> profile_m rewrite_ (rewrite r dn));
        free;
      }

  end 
  in
  A.store_ops
  

let _ 
: monad_ops:'t monad_ops ->
store_ops:('a, 'v, 't) store_ops ->
alloc:(unit -> ('a, 't) m) ->
evict:(('a * 'v) list -> (unit, 't) m) ->
write_back_cache_ops:('a, 'v, 'a * 'v, 'wb) Write_back_cache.write_back_cache_ops ->
with_write_back_cache:('wb, 't) with_state -> ('a, 'v, 't) store_ops
= add_write_back_cache_to_store
