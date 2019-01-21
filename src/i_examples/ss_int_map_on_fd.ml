(** A simple KV store, from small string to int, backed by a file *)

open Examples_common

module Internal = struct
  open Small_string
  open Bin_prot_util

  (* NOTE how easy it is to get readers and writers mixed up, and not
     agreeing with k_size and v_size *)
  let ps' (* ~blk_sz *) = 
    (Binprot_marshalling.mk_binprot_ps ())
      (* ~blk_sz *)
      ~cmp:SS.compare 
      ~k_size:bp_size_ss 
      ~v_size:bp_size_int
      ~read_k:bin_reader_ss
      ~write_k:bin_writer_ss
      ~read_v:bin_reader_int
      ~write_v:bin_writer_int

  let ps = ps' (* ~blk_sz *)

  let x : (ss,int) Internal.t = mk_example_on_fd ~ps
end
open Internal

let ps = Internal.ps
let map_ss_int = x

(* bind the fields so less faff later *)
let from_file = Examples_common.P.from_file map_ss_int
let map_ops = Examples_common.P.map_ops map_ss_int
let close = Examples_common.P.close map_ss_int
let ls_ops = Examples_common.P.ls_ops map_ss_int
