type ('k, 'v) pp = ('k, 'v) Prelude.Pickle_params.t
val tag_len : int
type 't free_ops = {
  get_free : unit -> (int, 't) Btree_api.m;
  set_free : int -> (unit, 't) Btree_api.m;
}
module BWP = Btree_with_pickle
val disk_to_store :
  Btree_api.BLK.sz ->
  't Btree_api.disk_ops ->
  ('k, 'v) BWP.pp ->
  't free_ops -> ('k, 'v, Btree_api.BLK.r, 't) Btree_api.store_ops
