(* a small KV store; keys and values are <=256 bytes *)

open Examples_common
open Small_string
open Bin_prot_util

let read_k = bin_reader_ss
let write_k = bin_writer_ss
let read_v = bin_reader_ss
let write_v = bin_writer_ss


let ps = Binprot_marshalling.mk_binprot_ps ~blk_sz
    ~cmp:SS.compare ~k_size:bp_size_ss ~v_size:bp_size_ss
    ~read_k ~write_k
    ~read_v ~write_v

let x = mk_example_on_fd ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x

