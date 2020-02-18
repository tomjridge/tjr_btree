(** Add LRU caching to an existing store; two versions: a read cache,
   and a write back cache. FIXME superseded by fs_shared write_back_cache?


- Read cache: writes are passed through in addition to being
   cached. So there is no need to flush anything. This dramatically
   improves performance.

- Write cache: writes are buffered; multiple writes to the same block
   commit when LRU overflows (using the last write). Using a write
   back cache improves performance even more. *)


(* FIXME move elsewhere eg fs_shared? *)

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


