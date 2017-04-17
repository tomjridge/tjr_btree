(* an lru cache ---------------------------------------- *)

(* we want to be able to take eg a map_ops and produce a cached
   version *)

open Prelude

type time = int

type dirty = bool

module Queue = Map_int

(* FIXME need polymorphic map - batteries? ext? *)
module Pmap = struct 
  type ('k,'v) t
  let map: ('v -> 'u) -> ('k,'v) t -> ('k,'u) t = fun _ -> failwith "FIXME"
  let bindings: ('k,'v) t -> ('k * 'v) list = fun  _ -> failwith "FIXME"
  let empty: ('k -> 'k -> int) -> ('k,'v) t = fun _ -> failwith "FIXME"
  let cardinal: ('k,'v) t -> int = fun _ -> failwith "FIXME"
  let iter: ('k -> 'v -> unit) -> ('k,'v) t -> unit = fun _ -> failwith "FIXME"
  let find: 'k -> ('k,'v) t -> 'v = fun _ -> failwith "FIXME"
  let remove: 'k -> ('k,'v) t -> ('k,'v) t = fun _ -> failwith "FIXME"
  let add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t = fun _ -> failwith "FIXME"
end

type ('k,'v) cache_state = {  
  max_size: int;
  current: time;
  map: ('k,'v option*time*dirty) Pmap.t;  
  (* None indicates known not to be present at lower (if
       dirty=false), or has been deleted (if dirty=true); Some v
       with dirty=true indicates that this needs to be flushed to lower *)
  queue: 'k Queue.t; (* map from time to key that was accessed at that time *)
}


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

(* a bit horrible! *)
let compare c1 c2 = (
  assert (c1.max_size = c2.max_size);
  (Pervasives.compare c1.current c2.current) |> then_
    (fun () -> Pervasives.compare 
        (c1.map |> Pmap.bindings)
        (c2.map |> Pmap.bindings)) |> then_
    (fun () -> Pervasives.compare
        (c1.queue |> Map_int.bindings)
        (c2.queue |> Map_int.bindings)))

let mk_initial_cache () = ({
    max_size=8;
    current=0;
    map=((Pmap.empty (fun _ -> failwith "FIXME")):('k,'v)Pmap.t);
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

(* FIXME make standalone state-passing, and then glue onto 'a m? *)

module Make = functor (Map_:Btree_api.Map) -> (struct
    module Map_ = Map_
    open Map_
    open W

    type ('k,'v) cache_ops = {
      get_cache: unit -> ('k,'v) cache_state m;
      put_cache: ('k,'v) cache_state -> unit m
    }

    (* for quick abort *)
    exception E_

    (* FIXME correctness of following not clear *)

    let make_cached_map :('k,'v) Map_.ops -> ('k,'v) cache_ops -> ('k,'v) Map_.ops = (
      fun map_ops cache_ops ->

        (* update time on each put *)
        let put_cache c = 
          let c = {c with current=c.current+1} in 
          cache_ops.put_cache c 
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
          cache_ops.get_cache () 
          |> Sem.bind (
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
          cache_ops.get_cache () 
          |> Sem.bind (
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
          cache_ops.get_cache () 
          |> Sem.bind (
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
        {find; insert; delete; get_leaf_stream}
    )

  end)

