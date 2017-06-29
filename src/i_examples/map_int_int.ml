(* a map from int to int, backed by file ------------------------------- *)

open Prelude
open Btree_api
open Frame
open Page_ref_int
open Examples_common
open Bin_prot_util

let read = Bin_prot.Std.bin_reader_int
let write = Bin_prot.Std.bin_writer_int
let sz = bin_size_int

(* this is generally useful, not just in examples *)
let ps' ~blk_sz = 
  Binprot_marshalling.mk_ps ~blk_sz
    ~cmp:Int_.compare ~k_size:sz ~v_size:sz
    ~read_k:read ~write_k:write
    ~read_v:read ~write_v:write

let ps = ps' ~blk_sz

let x = mk_example ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x
