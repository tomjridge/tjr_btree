type ('k, 'v) pp = ('k, 'v) Prelude.Pickle_params.t
module Page = Btree_api.BLK
val node_tag : int
val leaf_tag : int
val tag_len : int
val frame_to_page' :
  Page.sz -> ('k, 'v) pp -> ('k, 'v) Btree_api.Page_ref_int.frame -> Page.t
val page_to_frame' :
  ('k, 'v) pp -> Page.t -> ('k, 'v) Btree_api.Page_ref_int.frame
val frame_to_page :
  Page.sz -> ('a, 'b) pp -> ('a, 'b) Btree_api.Page_ref_int.frame -> Page.t
val page_to_frame :
  Page.sz -> ('a, 'b) pp -> Page.t -> ('a, 'b) Btree_api.Page_ref_int.frame
