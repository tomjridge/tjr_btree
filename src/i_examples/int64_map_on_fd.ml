(* a map from int64 to int64, backed by file ------------------------------- *)

open Prelude
open Btree_api
open Example_keys_and_values
open Btree_with_pickle
open Small_string.O
open Block.Blk4096

let pp = int64_int64_pp

let frame_to_page' blk_sz = bwp_frame_to_page blk_sz pp 
let page_to_frame' = bwp_page_to_frame blk_sz pp

let ps = 
  object
    method blk_sz=blk_sz
    method frame_to_page=frame_to_page'
    method page_to_frame=page_to_frame'
    method constants=Constants.make_constants blk_sz tag_len pp.k_len pp.v_len
    method cmp=Int64.compare
    method dbg_ps=None (* TODO *)
  end

let mk ~kk = Examples_common.mk_example ~ps ~kk
