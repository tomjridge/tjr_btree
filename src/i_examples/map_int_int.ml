(* a map from int to int, backed by file ------------------------------- *)

open Prelude
open Btree_api
open Frame
open Page_ref_int
open Examples_common

module Int_ = Bin_prot_int

let ps = 
  Binprot_marshalling.mk_ps ~blk_sz:4096 
    ~cmp:Int_.compare ~k_size:Int_.size ~v_size:Int_.size
    ~read_k:Int_.bin_reader_t ~write_k:Int_.bin_writer_t
    ~read_v:Int_.bin_reader_t ~write_v:Int_.bin_writer_t

let x = mk_example ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x
