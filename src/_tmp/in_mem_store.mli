type ('k, 'v) im = {
  free : int;
  map : ('k, 'v) Btree_api.Page_ref_int.frame Prelude.Map_int.t;
}
type ('k, 'v, 't) in_mem_ops = {
  get_store : unit -> (('k, 'v) im, 't) Btree_api.m;
  set_store : ('k, 'v) im -> (unit, 't) Btree_api.m;
}
val make :
  ('k, 'v, 't) in_mem_ops ->
  Constants.t -> ('k, 'v, Prelude.Map_int.key, 't) Btree_api.store_ops
