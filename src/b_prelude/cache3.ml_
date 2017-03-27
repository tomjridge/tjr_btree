(* an lru cache ---------------------------------------- *)

module type KEY_VALUE = Btree_api.KEY_VALUE

module type MAP = sig
  module KV : KEY_VALUE
  open KV
  type 'a m
  type map  (* a reference to some part of the state; actually some ops on kv  *)
  val find: map -> key -> value option m
  val insert: map -> key -> value -> unit m
  val delete: map -> key -> unit m
end

module type MONAD = sig
  type 'a m 
  type 'a ref_

  val bind: ('a -> 'b m) -> 'a m -> 'b m
  val return: 'a -> 'a m  
  val get: 'a ref_ -> 'a m
  val set: 'a ref_ -> 'a -> unit m
end

(* input signature *)
module type S = sig
  include MONAD
  include MAP with type 'a m := 'a m
end

module Make = functor (S:S) -> struct
  open Btree_api
  open Btree_util

  module S = S
  open S
  open S.KV

  module Map = Map.Make(
    struct 
      type t = key
      let compare x (y:key) = S.KV.key_ord x y
    end)

  type time = int
  type dirty = bool

  module Queue = Map_int

  type cache = {  (* state of cache *)
    max_size: int;
    current: time;
    map: (value option*time*dirty) Map.t;  
    (* None indicates known not to be present at lower, or has been
       deleted (depending on dirty) *)
    queue: key Queue.t; (* map from time to key that was accessed at that time *)
  }

  type cache_ref = cache S.ref_

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
     map=Map.map (fun (v,t,d) -> (v,Map_int.find t (!t_map),d)) c.map;
     queue=(!queue);
    }
  )
  
  let then_ f x = (if x=0 then f () else x)

  (* a bit horrible! *)
  let compare c1 c2 = (
    assert (c1.max_size = c2.max_size);
    (Pervasives.compare c1.current c2.current) |> then_
      (fun () -> Pervasives.compare 
          (c1.map |> Map.bindings)
          (c2.map |> Map.bindings)) |> then_
      (fun () -> Pervasives.compare
          (c1.queue |> Map_int.bindings)
          (c2.queue |> Map_int.bindings)))

  let initial_cache = ({
      max_size=8;
      current=0;
      map=Map.empty;
      queue=Queue.empty
    })
  
  type 'a m = 'a S.m

  (*
  let lens = Lens.{
      from=(fun x -> (x.store,x)); to_=(fun (s,x) -> { x with store=s })}

  let lift x = Sem.with_lens lens x
*)

  (* the cache never has more than max_size elts; the queue never has
     more than max_size elts 

     all k in map are in the queue; iff

     map and queue agree on timings

  *)
  let wf c = (
    Test.test (
      fun () -> 
        assert (Map.cardinal c.map <= c.max_size);
        assert (Queue.cardinal c.queue <= c.max_size);
        assert (Map.cardinal c.map = Queue.cardinal c.queue);
        Map.iter (fun k (v,t,d) -> 
            assert(Queue.find t c.queue = k)) c.map;
        ()
    )
  )

(*
  let get_cache: unit  ->  cache m = (fun () s -> (s,Ok s.cache))
  (* inc c.current on put *)
  let put_cache: cache -> unit  m = (fun c s -> 
      wf c;
      ({s with cache={c with current=c.current+1}},Ok()))
*)

  exception E_


  let evict r c = (
    let card = Map.cardinal c.map in
    match (card > c.max_size) with (* FIXME inefficient *)
    | false -> S.set r c
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
                 evictees:=(k,Map.find k c.map)::!evictees;
                 map:=Map.remove k !map;
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
                      S.delete k |> bind (fun () -> loop es) ))
              | Some v -> (
                  match dirty with
                  | false -> loop es
                  | true -> (
                      (* write out and continue *)
                      S.insert k v |> bind (fun () -> loop es
                                           )
                    ))))
        in
        lift (loop !evictees) |> bind (fun () ->
            let c' = {c with map=(!map); queue=(!queue)} in
            put_cache c' |> bind (fun () ->
                return () )))
  )

  (* FIXME following could be refactored a bit *)

  module Private_ = struct 

    let find k = (
      get_cache () |> Sem.bind (
        fun c ->
          (* try to find in cache *)
          try (
            let (v,time,dirty) = Map.find k c.map in          
            (* update time *)
            let time' = c.current in
            let c = {c with map=(Map.add k (v,time',dirty) c.map) } in
            (* remove entry from queue *)
            let c = {c with queue=(Queue.remove time c.queue) } in
            (* add new entry *)
            let c = {c with queue=(Queue.add time' k c.queue) } in
            (* update cache *)
            put_cache c |> bind (fun () -> 
                return v))
          with Not_found -> (
              (* retrieve from lower level *)
              lift (S.find k) |> bind (
                fun v -> 
                  (* update cache *)
                  let time = c.current in
                  let c = {c with map=(Map.add k (v,time,false) c.map) } in
                  (* update queue *)
                  let c = {c with queue=(Queue.add time k c.queue) } in
                  evict c |> bind (fun () -> 
                      return v))))
    )


    (* insert and delete *)
    let insert k v = (
      get_cache () |> Sem.bind (
        fun c -> 
          try (
            let (v_,time,dirty) = Map.find k c.map in          
            (* update time *)
            let time' = c.current in
            let c = {c with map=(Map.add k (Some v,time',dirty) c.map) } in
            (* remove entry from queue *)
            let c = {c with queue=(Queue.remove time c.queue) } in
            (* add new entry *)
            let c = {c with queue=(Queue.add time' k c.queue) } in
            (* update cache *)
            put_cache c |> bind (fun () -> 
                return ()))
          with Not_found -> (
              (* update cache *)
              let time = c.current in
              let dirty = true in
              let c = {c with map=(Map.add k (Some v,time,dirty) c.map) } in
              (* update queue *)
              let c = {c with queue=(Queue.add time k c.queue) } in
              evict c))
    )

    let delete k = (
      get_cache () |> Sem.bind (
        fun c -> 
          try (
            let (v_,time,dirty) = Map.find k c.map in          
            (* update time *)
            let time' = c.current in
            let dirty = true in
            let c = {c with map=(Map.add k (None,time',dirty) c.map) } in
            (* remove entry from queue *)
            let c = {c with queue=(Queue.remove time c.queue) } in
            (* add new entry *)
            let c = {c with queue=(Queue.add time' k c.queue) } in
            evict c |> bind (fun () -> 
                return ()))
          with Not_found -> (
              let time = c.current in
              let dirty = true in
              let c = {c with map=(Map.add k (None,time,dirty) c.map)} in
              (* add new entry to queue *)
              let c = {c with queue=(Queue.add time k c.queue) } in
              (* update cache *)            
              evict c))
    )

  end

  module type S_ = (sig
    val insert: key -> value -> unit m
    val find: key -> value option m
    val delete: key -> unit m 
  end)


  let _ = (module Private_: S_)

  include Private_

        
end

