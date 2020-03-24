(** Add LRU caching to an existing store; this version is a read cache
   (writes update the cache and go to disk immediately)
*)


(* FIXME move elsewhere eg fs_shared? *)

[%%import "config.ml"]

[%%if READ_CACHE_PROFILING_ENABLED]
let _ : unit = assert(Printf.printf "%s: profiling enabled\n%!" __FILE__; true)
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]

module Lru_profiler = struct
  open Tjr_profile
  let { mark; _ } = 
    if profiling_enabled then make_profiler ~print_header:(Printf.sprintf "bt lru profiler %s" __LOC__) ()
    else dummy_profiler
end
open Lru_profiler
    

(** NOTE this adds an imperative read cache; take care with testing etc *)
let add_imperative_read_cache_to_store (* make_store_with_lru *) (type blk_id node leaf) ~monad_ops ~store_ops =
  let module A = struct
    let ( >>= ) = monad_ops.bind 
    let return = monad_ops.return

    let [m1;m2;m3] = 
      ["m1";"m2";"m3"] |> List.map Tjr_profile.intern
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
          let equal : t -> t -> bool = Stdlib.(=)  (* FIXME don't use pervasives for real code *)
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
        (* FIXME may need to add promote explicitly in wrte and rewrite? *)
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


