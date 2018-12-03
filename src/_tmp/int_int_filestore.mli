module G = Generic_kv_store
val cs : int -> Constants.t
val mk_ps1 : int -> (int, int, Btree_api.BLK.r, G.t) Isa_util.Params_.ps1
val r2f : 'a option
val mk_maps_ops : int -> (int, int, G.t) Btree_api.map_ops
