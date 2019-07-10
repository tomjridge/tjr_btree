(** Add LRU caching to an existing store FIXME move elsewhere *)

open Tjr_profile

let profiler = Tjr_profile.make_string_profiler ()

let make_store_with_lru (type blk_id node leaf) ~monad_ops ~store_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in

  let profile_m ~mark s m = 
    return () >>= fun () -> 
    mark s;
    m >>= fun r ->
    mark (s^"'");
    return r
  in
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
    let mark = profiler.mark in
    {
      read=(fun r -> profile_m ~mark "ib" (read r));
      wrte=(fun dn -> profile_m ~mark "ic" (wrte dn));
      rewrite=(fun r dn -> profile_m ~mark "id" (rewrite r dn));
      free;
    }
  in
  store_ops
