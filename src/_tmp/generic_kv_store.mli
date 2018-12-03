val page_size : int
module T :
  sig
    type t = {
      fd : Disk_on_fd.fd;
      free : Btree_api.Page_ref_int.page_ref;
      root : Btree_api.Page_ref_int.page_ref;
    }
  end
type t =
  T.t = {
  fd : Disk_on_fd.fd;
  free : Btree_api.Page_ref_int.page_ref;
  root : Btree_api.Page_ref_int.page_ref;
}
module D = Disk_on_fd
module DS = Disk_to_store
module RS = Recycling_store
module SM = Store_to_map
val fd_ops : t D.fd_ops
val disk_ops : t Btree_api.disk_ops
val free_ops : t DS.free_ops
val mk_store_ops :
  ('a, 'b) DS.BWP.pp -> ('a, 'b, Btree_api.BLK.r, t) Btree_api.store_ops
val page_ref_ops : t SM.page_ref_ops
val mk_map_ops :
  ('a, 'b, SM.page_ref, t) SM.M.IU.Params_.ps1 ->
  (t -> ('a, 'b, SM.page_ref SM.M.Delete.finished) SM.M.IU.r2f) option ->
  ('a, 'b, t) Btree_api.map_ops
val mk_ps1 :
  Constants.t ->
  ('k -> 'k -> int) ->
  ('k, 'v) DS.BWP.pp -> ('k, 'v, Btree_api.BLK.r, t) Isa_util.Params_.ps1
val dummy : Disk_on_fd.fd -> t
val sz : Btree_api.BLK.sz
val frame_to_page :
  ('a, 'b) Btree_with_pickle.pp ->
  ('a, 'b) Btree_api.Page_ref_int.frame -> Btree_with_pickle.Page.t
val page_to_frame :
  ('a, 'b) Btree_with_pickle.pp ->
  Btree_with_pickle.Page.t -> ('a, 'b) Btree_api.Page_ref_int.frame
val write_root_block : t -> unit
val read_root_block : Disk_on_fd.fd -> int * int
val from_file :
  fn:string ->
  create:bool -> init:bool -> pp:('a, 'b) Btree_with_pickle.pp -> t
