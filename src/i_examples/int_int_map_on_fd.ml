(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api
open Example_keys_and_values
open Btree_with_pickle.O
open Small_string.O
open Block.Blk4096

let ps = 
  let pp = int_int_pp in
  object
    method blk_sz=blk_sz
    method pp=pp
    method constants=Constants.make_constants blk_sz tag_len pp.k_len pp.v_len
    method cmp=Int_.compare
    method dbg_ps=None (* TODO *)
  end

open Map_on_fd
open Map_on_fd.Default_implementation

let store_ops = mk_store_ops ~ps ~ops

let map_ops = 
  Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops

let ls_ops = mk_ls_ops ~ps ~page_ref_ops ~store_ops
