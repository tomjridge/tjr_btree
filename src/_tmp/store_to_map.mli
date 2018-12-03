type page_ref = int
type 't page_ref_ops = {
  get_page_ref : unit -> (page_ref, 't) Btree_api.m;
  set_page_ref : page_ref -> (unit, 't) Btree_api.m;
}
module M = Iter_with_check
module N = Iter_leaf_stream
val make_map_ops :
  ('k, 'v, page_ref, 't) M.IU.Params_.ps1 ->
  ('t -> ('k, 'v, page_ref M.Delete.finished) M.IU.r2f) option ->
  't page_ref_ops -> ('k, 'v, 't) Btree_api.map_ops
val make_ls_ops :
  ('k, 'v, page_ref, 't) N.IU.Params_.ps1 ->
  'a -> 't page_ref_ops -> ('k, 'v, page_ref, 't) Btree_api.ls_ops
