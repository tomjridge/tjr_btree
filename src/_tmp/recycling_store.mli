module Map_page_ref = Prelude.Map_int
module Cache = Map_page_ref
module FNS = Prelude.Set_int
type ('k, 'v) cache = ('k, 'v) Btree_api.Page_ref_int.frame Cache.t
type fns = FNS.t
type ('k, 'v) recycling_state = { cache : ('k, 'v) cache; fns : fns; }
type ('k, 'v, 'r, 't) lower_ops = {
  store_ops : ('k, 'v, 'r, 't) Btree_api.store_ops;
  store_alloc_page_ref : unit -> ('r, 't) Btree_api.m;
  store_write_frame :
    'r -> ('k, 'v) Btree_api.Page_ref_int.frame -> (unit, 't) Btree_api.m;
}
type ('k, 'v, 'r, 't) rs_ops = {
  store_ops : ('k, 'v, 'r, 't) Btree_api.store_ops;
  store_sync : unit -> (unit, 't) Btree_api.m;
}
type ('k, 'v, 't) rs_params = {
  get_rs : unit -> (('k, 'v) recycling_state, 't) Btree_api.m;
  set_rs : ('k, 'v) recycling_state -> (unit, 't) Btree_api.m;
}
val make :
  ('k, 'v, Cache.key, 't) lower_ops ->
  ('k, 'v, 't) rs_params -> ('k, 'v, Cache.key, 't) rs_ops
