(** An LRU cache on top of a map. *)

(* we want to be able to take eg a map_ops and produce a cached
   version *)

open Base_types
open Prelude
open Btree_api

module O = struct

  type time = int

  type dirty = bool

  module Queue = Map_int

  (* following needs polymorphic map - 
     batteries? no, only Pervasives.compare
     extlib? yes, allows parameterization by compare 
  *)

  module Pmap = struct 
    type ('k,'v) t = (*ExtLib.*) ('k,'v)PMap.t
    let map: ('v -> 'u) -> ('k,'v) t -> ('k,'u) t = PMap.map
    (* FIXME bindings and cardinal should be in ExtLib; also others *)
    let bindings: ('k,'v) t -> ('k * 'v) list = (fun m -> 
        let bs = ref [] in
        PMap.iter (fun k v -> bs:=(k,v)::!bs) m;
        List.rev !bs)
    let empty: ('k -> 'k -> int) -> ('k,'v) t = PMap.create
    let cardinal: ('k,'v) t -> int = (fun m ->
        let x = ref 0 in
        PMap.iter (fun k v -> x:=!x+1) m;
        !x)
    let iter: ('k -> 'v -> unit) -> ('k,'v) t -> unit = PMap.iter
    let find: 'k -> ('k,'v) t -> 'v = PMap.find
    let remove: 'k -> ('k,'v) t -> ('k,'v) t = PMap.remove
    let add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t = PMap.add
  end


  type ('k,'v) cache_state = {  
    max_size: int;
    current: time;
    map: ('k,'v option*time*dirty) Pmap.t;  
    queue: 'k Queue.t; (* map from time to key that was accessed at that time *)
  }
  (* for map, None indicates known not to be present at lower (if
     dirty=false), or has been deleted (if dirty=true); Some v with
     dirty=true indicates that this needs to be flushed to lower *)

  open Monad.Mref
  type ('k,'v,'t) cache_ops = ( ('k,'v)cache_state,'t) mref
end

open O
open Monad 


(* for testing, we typically need to normalize wrt. time *)
let normalize c = (
  (* we need to map times to times *)
  let t_map = ref Map_int.empty in
  let time = ref 0 in
  let queue = ref Queue.empty in
  let _ = 
    Queue.iter
      (fun t k -> 
         let t' = (!time) in
         t_map:=Map_int.add t t' (!t_map);
         queue:=Queue.add t' k (!queue);
         time:=!time+1;
         ())
      c.queue
  in
  {c with
   current=(!time);
   map=Pmap.map (fun (v,t,d) -> (v,Map_int.find t (!t_map),d)) c.map;
   queue=(!queue);
  }
)

let then_ f x = (if x=0 then f () else x)

(* FIXME a bit horrible! *)
let compare c1 c2 = (
  assert (c1.max_size = c2.max_size);
  (Pervasives.compare c1.current c2.current) |> then_
    (fun () -> Pervasives.compare 
        (c1.map |> Pmap.bindings)
        (c2.map |> Pmap.bindings)) |> then_
    (fun () -> Pervasives.compare
        (c1.queue |> Map_int.bindings)
        (c2.queue |> Map_int.bindings)))

let mk_initial_cache compare_k = ({
    max_size=8;
    current=0;
    map=((Pmap.empty compare_k):('k,'v)Pmap.t);
    queue=Queue.empty
  })


(* the cache never has more than max_size elts; the queue never has
     more than max_size elts 

     all k in map are in the queue; iff

     map and queue agree on timings

*)
let wf c = (
  Test.test (
    fun () -> 
      assert (Pmap.cardinal c.map <= c.max_size);
      assert (Queue.cardinal c.queue <= c.max_size);
      assert (Pmap.cardinal c.map = Queue.cardinal c.queue);
      Pmap.iter (fun k (v,t,d) -> 
          assert(Queue.find t c.queue = k)) c.map;
      ()
  )
)


(* for quick abort *)
exception E_

(* FIXME correctness of following not clear *)

open Monad
open Mref

let make_cached_map map_ops cache_ops : ('k,'v,'t) map_ops = (

  (* update time on each put *)
  let put_cache c = 
    let c = {c with current=c.current+1} in 
    cache_ops.set c 
  in

  let evict c = (
    let card = Pmap.cardinal c.map in
    match (card > c.max_size) with (* FIXME inefficient *)
    | false -> put_cache c
    | true -> (
        (* how many to evict? *)
        let n = card - (3 * c.max_size / 4) in
        (* for non-dirty, we just remove from map; for dirty we
           must flush to lower *)
        let count = ref 0 in
        let evictees = ref [] in
        let queue = ref c.queue in  
        let map = ref c.map in
        let _ = (
          try (
            Queue.iter 
              (fun time k -> 
                 queue:=Queue.remove time !queue;
                 evictees:=(k,Pmap.find k c.map)::!evictees;
                 map:=Pmap.remove k !map;
                 count:=!count +1;
                 if !count >= n then raise E_ else ())
              c.queue
          ) with E_ -> ())
        in
        (* now we have evictees, new queue, and new map *)
        let rec loop es = (
          match es with
          | [] -> return ()
          | e::es -> (
              let (k,(vopt,time,dirty)) = e in
              match vopt with 
              | None -> (
                  match dirty with
                  | false -> loop es
                  | true -> (
                      (* need to delete *)
                      map_ops.delete k |> bind (fun () -> loop es) ))
              | Some v -> (
                  match dirty with
                  | false -> loop es
                  | true -> (
                      (* write out and continue *)
                      map_ops.insert k v |> bind (fun () -> loop es) ))))
        in
        (loop !evictees) 
        |> bind (fun () ->
            let c' = {c with map=(!map); queue=(!queue)} in
            put_cache c' )))
  in

  let find k = (
    cache_ops.get () 
    |> bind (
      fun c ->
        (* try to find in cache *)
        try (
          let (v,time,dirty) = Pmap.find k c.map in          
          (* update time *)
          let time' = c.current in
          let c = {c with map=(Pmap.add k (v,time',dirty) c.map) } in
          (* remove entry from queue *)
          let c = {c with queue=(Queue.remove time c.queue) } in
          (* add new entry *)
          let c = {c with queue=(Queue.add time' k c.queue) } in
          (* update cache *)
          put_cache c |> bind (fun () -> return v))
        with Not_found -> (
            (* retrieve from lower level *)
            (map_ops.find k) 
            |> bind (
              fun v -> 
                (* update cache *)
                let time = c.current in
                let dirty = false in
                let c = {c with map=(Pmap.add k (v,time,dirty) c.map) } in
                (* update queue *)
                let c = {c with queue=(Queue.add time k c.queue) } in
                evict c |> bind (fun () -> return v)))))
  in

  let insert k v = (
    cache_ops.get () 
    |> bind (
      fun c -> 
        try (
          let (v_,time,dirty) = Pmap.find k c.map in          
          (* update time *)
          let time' = c.current in
          let dirty' = true in
          let c = {c with map=(Pmap.add k (Some v,time',dirty') c.map) } in
          (* remove entry from queue *)
          let c = {c with queue=(Queue.remove time c.queue) } in
          (* add new entry *)
          let c = {c with queue=(Queue.add time' k c.queue) } in
          (* update cache *)
          put_cache c)
        with Not_found -> (
            (* update cache *)
            let time = c.current in
            let dirty = true in
            let c = {c with map=(Pmap.add k (Some v,time,dirty) c.map) } in
            (* update queue *)
            let c = {c with queue=(Queue.add time k c.queue) } in
            evict c)))
  in

  let delete k = (
    cache_ops.get () 
    |> bind (
      fun c -> 
        try (
          let (v_,time,dirty) = Pmap.find k c.map in          
          (* update time *)
          let time' = c.current in
          let dirty' = true in
          let c = {c with map=(Pmap.add k (None,time',dirty') c.map) } in
          (* remove entry from queue *)
          let c = {c with queue=(Queue.remove time c.queue) } in
          (* add new entry *)
          let c = {c with queue=(Queue.add time' k c.queue) } in
          evict c)
        with Not_found -> (
            let time = c.current in
            let dirty = true in
            let c = {c with map=(Pmap.add k (None,time,dirty) c.map)} in
            (* add new entry to queue *)
            let c = {c with queue=(Queue.add time k c.queue) } in
            (* update cache *)            
            evict c)))
  in
  let get_leaf_stream () = failwith "FIXME" in
  {find; insert; delete}
)
