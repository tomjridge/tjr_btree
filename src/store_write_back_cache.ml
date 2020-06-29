(** Add a write back cache to a store *)

open Profilers_.Write_back_cache_profiler

(** NOTE alloc is necessary because a write of a node may only
   allocate the block, rather than actually writing.

NOTE that eviction should be handled asynchronously, by repeatedly
   calling wrte on the lower store_ops. Obviously evicted writes
   should not be overtaken by writes to the same blocks (the lower
   store should be accessed serially).  
*)

let add_write_back_cache_to_store (type blk_id wb)
    ~monad_ops ~uncached_store_ops 
    ~alloc 
    ~(evict:(blk_id * 'dnode)list -> (unit,'t)m)
    ~(write_back_cache_ops:(blk_id,'dnode,wb) Write_back_cache.wbc_ops)
    ~with_write_back_cache 
  = 
  let module A = struct
    let ( >>= ) = monad_ops.bind 
    let return = monad_ops.return

    let [read_;wrte_;rewrite_] = 
      ["read";"wrte";"rewrite"] |> List.map Tjr_profile.intern
    [@@ocaml.warning "-8"]

(*
    module L = Write_back_cache.Make(
      struct
        type t = blk_id
        let compare : t -> t -> int = Stdlib.compare  
        (* $(FIXME("""don't use pervasives for real code""")) *)
      end)
*)

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
      let Store_ops.{read;wrte=_NOTE_NOT_USED;rewrite=_NOTE_NOT_USED';free} = 
        uncached_store_ops 
      in
      (* Add some memoization *)
      let wb = write_back_cache_ops in
      (* lift trim_if_over_cap to monad FIXME rename to trim_and_set_cache *)
      let trim_and_set_cache ~set_state cache = 
        (if wb.needs_trim cache then wb.trim cache else [],cache) |> function
        | [],cache -> set_state cache
        | evictees,cache' -> (
            evict evictees >>= fun () -> 
            set_state cache')
      in
      let read = fun r -> 
        with_write_back_cache.with_state (fun ~state:cache ~set_state -> 
            wb.find r cache |> function
            | None,cache' -> (
                (* let r' : int = Obj.magic r in *)
                (* Printf.printf "Attempt to read blk %d\n%!" r'; *)
                (* read and insert a clean entry into the cache *)
                read r >>= fun dn -> 
                wb.insert r (dn,false) cache' |> fun cache' -> 
                trim_and_set_cache ~set_state cache' >>= fun () -> return dn)
            | Some (dn,_dirty),cache' -> (
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
            | None,_ -> (
                (* assume we can't rewrite *)
                alloc_and_place_dirty_entry_in_cache ())
            | Some (_dn,dirty),cache' -> (  (* was dn rather than _dn! nasty bug *)
                match dirty with
                | true -> (
                    wb.insert r (dn,true) cache' |> fun cache' -> 
                    set_state cache' >>= fun () -> return None)
                | false -> (
                    alloc_and_place_dirty_entry_in_cache ()))
          )
      in
      let free blk_ids = 
        print_endline "Call to free";
        ignore(failwith __LOC__ : unit); (* FIXME remove *)
        (* these blocks are guaranteed to never be accessed again; so
           we can remove them from the cache *)
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
      Store_ops.{
        read=(fun r -> profile_m read_ (read r));
        wrte=(fun dn -> profile_m wrte_ (wrte dn));
        rewrite=(fun r dn -> profile_m rewrite_ (rewrite r dn));
        free;
      }

  end 
  in
  A.store_ops


let _ :
monad_ops:'t Tjr_monad.monad_ops ->
uncached_store_ops:('blk_id, 'dnode, 't) Isa_btree.store_ops ->
alloc:(unit -> ('blk_id, 't) Tjr_monad.m) ->
evict:(('blk_id * 'dnode) list -> (unit, 't) Tjr_monad.m) ->
write_back_cache_ops:('blk_id, 'dnode, 'wb)
                     Tjr_fs_shared.Write_back_cache.wbc_ops ->
with_write_back_cache:('wb, 't) Tjr_monad.with_state ->
('blk_id, 'dnode, 't) Isa_btree.store_ops
  = add_write_back_cache_to_store
