(* testing cache ---------------------------------------- *)

open Base_types
open Cache
open Map_ops

(* we test just cache behaviour, not linked with btree *)

(* in memory *)

(* note that the use of time means that we need to normalize timings
   (and current time) in order to exhaust state space *)

type key = int
type value = int
type op = Find of key | Insert of key * value | Delete of key


module Test_state = struct 

  type t = {
    spec: int Map_int.t;
    cache: (key,value)cache_state;
    base_map: int Map_int.t;
  }

  let then_ f x = (if x=0 then f () else x)

  (* FIXME v inefficient *)
  let compare s1 s2 = 
    (Pervasives.compare 
       (s1.spec |> Map_int.bindings) (s2.spec |> Map_int.bindings)) |> then_
      (fun () -> 
         Cache.compare s1.cache s2.cache) |> then_
      (fun () -> 
         Map_int.compare Pervasives.compare s1.base_map s2.base_map)

end

open Test_state

let init_cache = Cache.mk_initial_cache Int_.compare |> Cache.normalize

let init_base_map = Map_int.empty

let init_spec = Map_int.empty

let initial_state = { spec=init_spec; cache=init_cache; base_map=init_base_map }


(* base uncached map ------------------------------------------------ *)

let with_state = Tjr_step_monad.Extra.with_state

(* FIXME why is find defined in the following, but others are not? *)
let base_map_ops : ('k,'v,'t) map_ops = 
  let find k =
    with_state @@ fun t -> 
    let v = try Some(Map_int.find k t.base_map) with _ -> None in
    (v,t)
  in
  let insert k v = failwith __LOC__ in
  let insert_many k v kvs = failwith __LOC__ in
  let delete k = failwith __LOC__ in
  mk_map_ops ~find ~insert ~delete ~insert_many


(* cached map ------------------------------------------------------- *)

let cache_ops = {
  get=(fun () -> with_state (fun t -> (t.cache,t)));
  set=(fun cache -> with_state (fun t -> ((),{t with cache})))
}

let cached_map_ops = 
  Cache.make_cached_map ~map_ops:base_map_ops ~cache_ops @@
  fun ~cached_map_ops ~evict_hook -> cached_map_ops

let _ = cached_map_ops

let (find,insert,delete) = 
  dest_map_ops cached_map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  (find,insert,delete)


(* exhaustive testing ----------------------------------------------- *)

(* we use module Exhaustive *)

open Exhaustive

let run = Tjr_step_monad.Extra.run

let step t op =
  begin
    match op with
    | Find k -> find k |> run t |> (fun (t',_) -> t')
    | Insert (k,v) -> 
      insert k v 
      |> run t
      |> (fun (t',_) -> {t' with spec=Map_int.add k v t'.spec})
    | Delete k -> 
      delete k
      |> run t
      |> (fun (t',_) -> {t' with spec=Map_int.remove k t'.spec})
  end                   
  |> (fun x -> [{ x with cache=Cache.normalize x.cache}])

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

   we already check internal invariants of course
*)

let check_state x = () (* FIXME TODO *)
let check_step x y = () (* FIXME TODO *)

let test_ops = { step; check_state; check_step }


(* sets of states --------------------------------------------------- *)

module Ord = struct 
  type t = Test_state.t
  let compare (x:t) (y:t) = Test_state.compare x y
end

module Set_ops = Tjr_set.Make_set_ops(Ord)

let set_ops = Set_ops.set_ops


(* running exhaustive tests ---------------------------------------- *)

(* let range = BatList.(range 1 `To 5) *)


let test range =
  let ops =
    range 
    |> List.map (fun k -> [Find k;Insert(k,2*k); Delete k])
    |> List.concat
  in
  Printf.printf "%s: " __MODULE__;
  test ~set_ops ~test_ops ops [initial_state];
  print_string "\n\n";



(* old ============================================================ *)

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


