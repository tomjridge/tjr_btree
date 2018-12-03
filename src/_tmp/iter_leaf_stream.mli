module IU = Isa_util
val next_leaf :
  ('k, 'v, 'r, 't) IU.Params_.ps1 ->
  ('k, 'v, 'r) Btree_api.lss -> (('k, 'v, 'r) Btree_api.lss option, 't) IU.m
val mk_leaf_stream :
  ('k, 'v, 'r, 't) IU.Params_.ps1 ->
  'r -> (('k, 'v, 'r) Btree_api.lss, 't) IU.m
val ls_kvs : ('k, 'v, 'a) Btree_api.lss -> ('k * 'v) list
val ls_step :
  ('a, 'b, 'c, 'd) IU.Params_.ps1 ->
  ('a, 'b, 'c) Btree_api.lss -> (('a, 'b, 'c) Btree_api.lss option, 'd) IU.m
