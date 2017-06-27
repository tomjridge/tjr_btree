(** A map from small string to int, backed by file descriptor *)

open Prelude
open Btree_api
open Block.Blk4096
open Page_ref_int
open Examples_common

module SS = Bin_prot_ss
module Int_ = Bin_prot_int

let ps = 
  Binprot_marshalling.mk_ps ~blk_sz:4096
    ~cmp:SS.compare ~k_size:SS.size ~v_size:Int_.size
    ~read_k:SS.bin_reader_t ~write_k:SS.bin_writer_t
    ~read_v:Int_.bin_reader_t ~write_v:Int_.bin_writer_t

let x = Examples_common.mk_example ~ps
