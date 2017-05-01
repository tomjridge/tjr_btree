(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api
open Example_keys_and_values
open Btree_with_pickle.O
open Small_string.O
open Default

let ps store_ops = 
  let pp = int_int_pp in
  object
    method block_size=default_blk_sz
    method pp=pp
    method constants=Constants.make_constants default_blk_sz tag_len pp.k_len pp.v_len
    method compare_k=Int.compare
    method debug=None (* TODO *)
    method store_ops=store_ops
  end

let store_ops = Map_on_fd.mk_store_ops (ps ())

let ps = ps store_ops

let map_ops = Map_on_fd.mk_map_ops ps

let ls_ops = Map_on_fd.mk_ls_ops ps
