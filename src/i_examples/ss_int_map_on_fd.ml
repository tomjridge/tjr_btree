(** A map from small string to int, backed by file descriptor *)

open Prelude
open Btree_api
open Example_keys_and_values
open Btree_with_pickle.O
open Small_string.O
open Default

let ps store_ops = 
  let pp = ss_int_pp in
  object
    method blk_sz=blk_sz
    method pp=pp
    method constants=Constants.make_constants blk_sz tag_len pp.k_len pp.v_len
    method compare_k=SS.compare
    method debug=None (* TODO *)
    method store_ops=store_ops
  end

open Map_on_fd.Default_implementation

let store_ops = mk_store_ops (ps ())

let ps = ps store_ops

let map_ops = mk_map_ops ps

let ls_ops = mk_ls_ops ps
