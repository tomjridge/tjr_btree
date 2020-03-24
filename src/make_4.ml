(** Like {!Make_3} but with {!Std_types} *)


open Tjr_fs_shared.Std_types
(* open Btree_intf *)
open Make_3

class type ['k,'v] args = object
  method k_cmp: 'k -> 'k -> int
  method cs: Isa_btree.Constants.constants
  method k_mshlr: 'k bin_mshlr
  method v_mshlr: 'v bin_mshlr
end

module R_mshlr = struct
  type t = r[@@deriving bin_io]
  let max_sz = 9
end

let r_mshlr : blk_id bin_mshlr = (module R_mshlr)
  
(**/**)
let make ~(args:('k,'v)args) ~(blk_dev_ops:std_blk_dev_ops) ~blk_alloc ~root_ops =
  let args:(_,_,_,_)Make_3.args = object
    method monad_ops=monad_ops
    method k_cmp=args#k_cmp
    method blk_sz=blk_sz
    (* method cs=args#cs *)
    method k_mshlr=args#k_mshlr
    method v_mshlr=args#v_mshlr
    method r_mshlr=(failwith "")
    method with_read_cache=true
  end
  in
  Make_3.make_uncached ~args ~blk_dev_ops ~blk_alloc ~root_ops
(**/**)

let make : 
args:('k, 'v) args ->
blk_dev_ops:std_blk_dev_ops ->
blk_alloc:(blk_id, t) blk_allocator_ops ->
root_ops:(blk_id, t) with_state ->
('k, 'v, t) uncached_btree
= make
(** {[
args:('k, 'v) args ->
blk_dev_ops:std_blk_dev_ops ->
blk_alloc:(blk_id, t) blk_allocator_ops ->
root_ops:(blk_id, t) with_state ->
('k, 'v, t) uncached_btree
]} *)
