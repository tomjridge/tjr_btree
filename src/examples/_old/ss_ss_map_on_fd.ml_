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
let map_ss_ss = x

(* extract some fields *)
let from_file = Examples_common.P.from_file map_ss_ss
let map_ops = Examples_common.P.map_ops map_ss_ss
let close = Examples_common.P.close map_ss_ss
let ls_ops = Examples_common.P.ls_ops map_ss_ss
