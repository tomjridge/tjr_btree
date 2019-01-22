(** A simple KV store, from int to int, backed by file *)

open Examples_common

module Internal = struct
  open Bin_prot_util 

  (* this is generally useful, not just in examples *)
  let ps' (*~blk_sz *) = 
    (Binprot_marshalling.mk_binprot_ps ())
      ~cmp:Tjr_int.compare 
      ~k_size:bp_size_int
      ~v_size:bp_size_int
      ~read_k:bin_reader_int
      ~write_k:bin_writer_int
      ~read_v:bin_reader_int 
      ~write_v:bin_writer_int

  let ps = ps' (* ~blk_sz *)

  let x : (int,int) Internal.t = mk_example_on_fd ~ps
end
open Internal

let ps = ps
let map_int_int = x

(* these are the particular fields of map_int_int *)

let from_file = Examples_common.P.from_file x
let map_ops = Examples_common.P.map_ops x
let close = Examples_common.P.close x
let ls_ops = Examples_common.P.ls_ops x

