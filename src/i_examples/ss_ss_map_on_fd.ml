(* A simple KV store backed by a file; keys and values are <=256 bytes *)

open Examples_common
module Internal = struct
  open Small_string
  open Bin_prot_util

  let ps = 
    Binprot_marshalling.mk_binprot_ps 
      ~blk_sz
      ~cmp:SS.compare 
      ~k_size:bp_size_ss 
      ~v_size:bp_size_ss
      ~read_k:bin_reader_ss
      ~write_k:bin_writer_ss
      ~read_v:bin_reader_ss
      ~write_v:bin_writer_ss
  
  let x : (ss,ss,int,int) Internal.t = mk_example_on_fd ~ps
end
open Internal

let ps = ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x

