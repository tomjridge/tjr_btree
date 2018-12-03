module Default_block :
  sig
    type t
    type r = int
    type sz = int
    val of_string : sz -> string -> t
    val to_string : t -> string
  end
module BLK = Default_block
type ('a, 't) m = ('a, 't) Simple_monad.m
type 'k ord = 'k -> 'k -> int
type 't disk_ops = {
  block_size : BLK.sz;
  read : BLK.r -> (BLK.t, 't) m;
  write : BLK.r -> BLK.t -> (unit, 't) m;
  disk_sync : unit -> (unit, 't) m;
}
type ('k, 'v, 'r, 't) store_ops = {
  cs0 : Constants.t;
  store_free : 'r list -> (unit, 't) m;
  store_read : 'r -> (('k, 'v, 'r) Frame.frame, 't) m;
  store_alloc : ('k, 'v, 'r) Frame.frame -> ('r, 't) m;
}
type ('k, 'v, 't) map_ops = {
  find : 'k -> ('v option, 't) m;
  insert : 'k -> 'v -> (unit, 't) m;
  delete : 'k -> (unit, 't) m;
}
type ('k, 'v, 'r) lss = {
  kvs : ('k * 'v) list;
  ls : ('k, 'v, 'r) Isa_util.ls_state;
}
type ('k, 'v, 'r, 't) ls_ops = {
  mk_leaf_stream : unit -> (('k, 'v, 'r) lss, 't) m;
  ls_step : ('k, 'v, 'r) lss -> (('k, 'v, 'r) lss option, 't) m;
  ls_kvs : ('k, 'v, 'r) lss -> ('k * 'v) list;
}
val all_kvs : ('k, 'v, 'r, 't) ls_ops -> (('k * 'v) list, 't) m
module Page_ref_int :
  sig
    type page_ref = int
    type ('k, 'v) frame = ('k, 'v, page_ref) Frame.frame
  end
