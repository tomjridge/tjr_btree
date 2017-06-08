(** A map from small string to int, backed by file descriptor *)

open Prelude
open Btree_api
open Example_keys_and_values
open Btree_with_pickle
open Small_string.O
open Block.Blk4096
open Page_ref_int

let pp = ss_int_pp

let frame_to_page' blk_sz = bwp_frame_to_page blk_sz pp 
let page_to_frame' = bwp_page_to_frame blk_sz pp

let ps = 
  object
    method blk_sz=blk_sz
    method frame_to_page=frame_to_page'
    method page_to_frame=page_to_frame'
    method constants=Constants.make_constants blk_sz 4 260 4 (* TODO *)
    method cmp=SS.compare
    method dbg_ps=None (* TODO *)
  end

open Map_on_fd
open Map_on_fd.Default_implementation

let store_ops = mk_store_ops ~ps ~ops

let map_ops = 
  Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops

let ls_ops = mk_ls_ops ~ps ~page_ref_ops ~store_ops
