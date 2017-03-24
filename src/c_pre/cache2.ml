(* an lru cache ---------------------------------------- *)

module type KEY_VALUE = Btree_api.KEY_VALUE

module type MAP = sig
  type 'a m
  type ('k,'v) map  (* a reference to a block  *)
  val find: ('k,'v) map -> 'k -> 'v option m
  val insert: ('k,'v) map -> 'k -> 'v -> unit m
  val delete: ('k,'v) map -> 'k -> unit m
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

(* cache *)


(* we need a polymorphic map, but functorized over the key compare
   function; PMap in extlib *)

module Make = functor (S:S) -> struct

(* FIXME should be in result, not in input? but we need to somehow bind to state *)

  module S = S

  module Queue = Btree_util.Map_int

  type time = int
  type dirty = bool

  this isn't working; need to try again with functors

  module PMap = struct
    open Btree_util
    type ('k,'v) t = { 
      find: 'k -> 'v; 
      add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t;
      bindings: ('k*'v) list;
      map: 'u. ('v -> 'u) -> ('k,'u) t;
    }

  end
    
  type ('k,'v) cache = {  (* state of cache *)
    max_size: int;
    current: time;
    map: ('k, 'v option*time*dirty) PMap.t;  
    (* None indicates known not to be present at lower, or has been
       deleted (depending on dirty) *)
    queue: 'k Queue.t; (* map from time to key that was accessed at that time *)
  }


  (* the eventual result *)
  module T_ (* : S *) = (struct
    open S
    open Btree_util

    (* debugging ---------------------------------------- *)

    (* for testing, we typically need to normalize wrt. time *)

    let normalize m c = (
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
       map=c.map.map (fun (v,t,d) -> (v,Map_int.find t (!t_map),d));
       queue=(!queue);
      }
    )

    let then_ f x = (if x=0 then f () else x)

    (* a bit horrible! for debugging *)
    let compare c1 c2 = (
      assert (c1.max_size = c2.max_size);
      (Pervasives.compare c1.current c2.current) |> then_
        (fun () -> Pervasives.compare 
            (c1.map.bindings)
            (c2.map.bindings)) |> then_
        (fun () -> Pervasives.compare
            (c1.queue |> Map_int.bindings)
            (c2.queue |> Map_int.bindings)))

    (* initial cache ---------------------------------------- *)
    
    let initial_cache (type k) (comp: k -> k -> int) = (
      let module M = Map.Make(struct type t = k let compare = comp end) in
      {
        max_size=8;
        current=0;
        map=M.empty;
        queue=Queue.empty
      ())


(*      
{
        max_size=8;
        current=0;
        map=PMap_.create compare_;
        queue=Queue.empty
      })
*)
          

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

    exception E_

    open C

    let evict c = (
      let card = Map.cardinal c.map in
      match (card > c.max_size) with (* FIXME inefficient *)
      | false -> set_cache c
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
                   (* exit quickly *)
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
          (loop !evictees) |> bind (fun () ->
              let c' = {c with map=(!map); queue=(!queue)} in
              set_cache c' |> bind (fun () ->
                  return () )))
    )

    (* FIXME following could be refactored a bit *)

    let find k = (
      get_cache () |> bind (
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
            set_cache c |> bind (fun () -> 
                return v))
          with Not_found -> (
              (* retrieve from lower level *)
              (S.find k) |> bind (
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
      get_cache () |> bind (
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
            set_cache c |> bind (fun () -> 
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
      get_cache () |> bind (
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

  end) (* T_ *)

  let _ = (module T_ : KV_MAP)

  include T_
        
end

