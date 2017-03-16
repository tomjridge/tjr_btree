(* testing cache ---------------------------------------- *)

(* in memory *)

(* note that the use of time means that we need to normalize timings
   (and current time) in order to exhaust state space *)

open Btree_api
open Sem
open Btree_util

module KV = Map_int_int.KV
let _ = (module KV : Btree_api.KEY_VALUE)

open KV

module Op = struct
  type t = Find of key | Insert of key * value | Delete of key
end

type op = Op.t

(* our sample store ---------------------------------------- *)

module ST (* : S *) = struct
  module KV = KV

  type map = int Map_int.t

  type t = { map: map; (* history: op list *) }

  type 'a m = ('a,t) Sem.m

  let get_map : unit -> map m = (fun () s -> (s,Ok s.map))
  let put_map : map -> unit m = (fun t s -> ({map=t},Ok ()))
  let put_op: op->unit m = 
    (* (fun op s -> ({s with history=s.history@[op]},Ok())) *)
    fun op s -> (s,Ok())

  let find: key -> value option m = (
    fun k -> 
      put_op (Find k) |> bind (fun () -> 
          get_map () |> bind (fun m ->
              try (
                return (Some(Map_int.find k m)))
              with Not_found -> (
                  return None
                )
            ))
  )

  let insert: key -> value -> unit m = (
    fun k v -> 
      put_op (Insert(k,v)) |> bind (fun () -> 
          get_map () |> bind (fun m ->
              let m' = Map_int.add k v m in
              put_map m'))
  )
        
  let delete: key -> unit m = (
    fun k -> 
      put_op (Delete(k)) |> bind (fun () -> 
          get_map() |> bind (fun m -> 
              put_map (Map_int.remove k m)))
  )

  let initial_state = { map=Map_int.empty; (* history=[]*) }

end

let _ = (module ST : Cache.S)

module Cache_ = Cache.Make(ST)


(* exhaustive testing ---------------------------------------- *)

module Test_state = struct 
  type t = {
    spec: int Map_int.t;
    cache: Cache_.c_t;
    store: ST.t
  }

  let then_ f x = (if x=0 then f () else x)

  (* FIXME v inefficient *)
  let compare s1 s2 = (
    (Pervasives.compare 
       (s1.spec |> Map_int.bindings) (s2.spec |> Map_int.bindings)) |> then_
      (fun () -> 
         Cache_.compare s1.cache s2.cache) |> then_
      (fun () -> 
         Map_int.compare Pervasives.compare s1.store.map s2.store.map))


  let init_cache = { Cache_.initial_cache with max_size=4} |> Cache_.normalize
                        
  let init_store = ST.initial_state
                        
  let init_spec = Map_int.empty

  let initial_state = { spec=init_spec; cache=init_cache; store=init_store }

end

let _ = (module Test_state: Set.OrderedType)


module S = struct
  module State = Test_state
  open State

  type t = State.t
  type op = Op.t

  open Op

  
  let step op t = (
    let (c,s) = (t.cache,t.store) in
    let post x = 
      x 
      |> Sem.run Cache_.{cache=c;store=s}
      |> (fun (x,Ok _) -> {t with cache=x.cache; store=x.store})
    in
    match op with
    | Find k -> (Cache_.find k |> post)
    | Insert (k,v) -> (
        Cache_.insert k v 
        |> post 
        |> (fun t' -> {t' with spec=Map_int.add k v t'.spec}))
    | Delete k -> (
        Cache_.delete k
        |> post
        |> (fun t' -> {t' with spec=Map_int.remove k t'.spec}))
  ) |> (fun x -> [{ x with cache=Cache_.normalize x.cache}])

(* cache invariants:

   the model corresponds to the store plus the cache

   if an entry in the cache is not dirty, then the same entry exists in store

   the cache map:none then if dirty then k exists in store, otherwise
   doesn't exist in store

   the key involved in a step is at the head of hte queue (unless
   delete?); the queue is like the queue at the previous step, but k
   may have bene deleted; evicted elements may also have disappeared
   (?) this looks like we need to do some more work here

   FIXME these are not currently checked
*)

  let check_invariants t = ()  (* FIXME *)
  let check_step_invariants t t' = ()  (* FIXME *)
end

module E_ = Exhaustive.Make(S)


(* running exhaustive tests ---------------------------------------- *)

let range = BatList.(range 1 `To 5)

let ops = Op.(
  range 
  |> List.map (fun k -> [Find k;Insert(k,2*k); Delete k])
  |> List.concat)

let main () = (
  Printf.printf "%s: " __MODULE__;
  E_.test ops (E_.STS.singleton Test_state.initial_state);
  ()
)

(* let _ = main () *)

(* with range 1..5 takes a while:

todo: 14534; done: 1729999 ..........
todo: 11166; done: 1739999 .
*)


(* manual interactive testing ---------------------------------------- *)

(*

(* to test, we need to track the operations *)

let initial_cache = Cache_.initial_cache

open Cache_

(* insert 8 elts *)

let s0 = ref { cache=initial_cache; store=ST.initial_state }

let run = Sem.run_ref s0

let _ = 
  for x = 1 to 8 do
    run (Cache_.insert x x)
  done

(* have a look at the state *)
let _ = !s0 

let _ = Map.bindings (!s0).cache.map

let _ = run (Cache_.insert 9 9)


let _ = Map.bindings (!s0).cache.map
let _ = Queue.bindings (!s0).cache.queue
let _ = Map_int.bindings (!s0).store.map

let _ = run (Cache_.insert 3 3)

let _ = run (Cache_.insert 11 11)
let _ = run (Cache_.find 1|> bind (fun _ -> return ()))

let _ = run (Cache_.insert 12 12)


*)
