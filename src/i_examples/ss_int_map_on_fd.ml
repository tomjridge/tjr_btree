(** A map from small string to int, backed by file descriptor *)

open Btree_api
open Block.Blk4096
open Page_ref_int
open Examples_common
open Small_string
open Bin_prot_util

let read_k = bin_reader_ss
let write_k = bin_writer_ss
let read_v = bin_reader_int
let write_v = Bin_prot.Std.bin_writer_int

let ps' ~blk_sz = 
  Binprot_marshalling.mk_ps ~blk_sz
    ~cmp:SS.compare ~k_size:bp_size_ss ~v_size:bp_size_int
    ~read_k ~write_k
    ~read_v ~write_v

let ps = ps' ~blk_sz

let x = Examples_common.mk_example ~ps
