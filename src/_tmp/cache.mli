type time = int
type dirty = bool
module Queue = Prelude.Map_int
module Pmap :
  sig
    type ('k, 'v) t = ('k, 'v) PMap.t
    val map : ('v -> 'u) -> ('k, 'v) t -> ('k, 'u) t
    val bindings : ('k, 'v) t -> ('k * 'v) list
    val empty : ('k -> 'k -> int) -> ('k, 'v) t
    val cardinal : ('k, 'v) t -> int
    val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
    val find : 'k -> ('k, 'v) t -> 'v
    val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
    val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  end
type ('k, 'v) cache_state = {
  max_size : int;
  current : time;
  map : ('k, 'v option * time * dirty) Pmap.t;
  queue : 'k Queue.t;
}
val normalize : ('a, 'b) cache_state -> ('a, 'b) cache_state
val then_ : (unit -> int) -> int -> int
val compare : ('a, 'b) cache_state -> ('a, 'b) cache_state -> int
val mk_initial_cache : ('k -> 'k -> int) -> ('k, 'a) cache_state
val wf : ('a, 'b) cache_state -> unit
type ('k, 'v, 't) cache_ops = {
  get_cache : unit -> (('k, 'v) cache_state, 't) Btree_api.m;
  set_cache : ('k, 'v) cache_state -> (unit, 't) Btree_api.m;
}
type ('k, 'v, 't) map_ops = {
  find : 'k -> ('v option, 't) Btree_api.m;
  insert : 'k -> 'v -> (unit, 't) Btree_api.m;
  delete : 'k -> (unit, 't) Btree_api.m;
}
exception E_
val make_cached_map :
  ('k, 'v, 't) map_ops -> ('k, 'v, 't) cache_ops -> ('k, 'v, 't) map_ops
