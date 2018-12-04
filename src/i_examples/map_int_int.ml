(** A simple KV store, from int to int, backed by file *)

open Examples_common

module Internal = struct
  open Bin_prot_util 

  (* this is generally useful, not just in examples *)
  let ps' ~blk_sz = 
    Binprot_marshalling.mk_binprot_ps 
      ~blk_sz
      ~cmp:Tjr_int.compare 
      ~k_size:bp_size_int
      ~v_size:bp_size_int
      ~read_k:bin_reader_int
      ~write_k:bin_writer_int
      ~read_v:bin_reader_int 
      ~write_v:bin_writer_int

  let ps = ps' ~blk_sz

  let x : (int,int,int,int) Internal.t = mk_example_on_fd ~ps
end
open Internal

let ps = ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x


