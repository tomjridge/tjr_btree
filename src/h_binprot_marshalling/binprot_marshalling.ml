open Prelude
open Btree_api
(*module Blk = Block.Blk4096
open Blk*)
open Frame
open Page_ref_int

module BP = Bin_prot

open BP.Std

module BlkN = Block.BlkN


type ('k,'v) binprot_tree  = 
    N of 'k list * page_ref list 
  | L of ('k*'v) list [@@deriving bin_io]

let f2bp frm = (
  match frm with
  | Node_frame (ks,rs) -> N (ks,rs)
  | Leaf_frame kvs -> L kvs)

let bp2f iis = (
  match iis with
  | N (ks,rs) -> Node_frame(ks,rs)
  | L kvs -> Leaf_frame kvs)

open Bigarray
type buf = BP.Common.buf

(* pull this out because frame_to_page takes an explicit blk_sz; FIXME
   should it? *)
let frm_to_pg ~write_k ~write_v ~blk_sz = 
  let bin_writer' = (bin_writer_binprot_tree write_k write_v) in
  fun (frm:('k,'v)frame) -> (
      let buf = BP.Common.create_buf blk_sz in
      let pos' = frm |> f2bp |> bin_writer'.write buf ~pos:0 in
      let s = Bytes.create blk_sz in
      let () = BP.Common.blit_buf_string buf s pos' in
      s |> BlkN.of_string blk_sz
    )

let mk_ps ~blk_sz = 

  let pg_to_frm ~read_k ~read_v = 
    let bin_reader' = bin_reader_binprot_tree read_k read_v in
    fun pg -> (
        let s = pg |> BlkN.to_string in
        let buf = BP.Common.create_buf blk_sz in
        let _ = BP.Common.blit_string_buf s buf blk_sz in
        let bp = bin_reader'.read buf (ref 0) in
        bp|>bp2f)
  in

  let _ = assert (Sys.int_size = 63) in (* ensure we are on 64 bit system *)

  (* 

* doc: bin_prot marshalling

  - a size (Bin_prot.Nat0.t) < 0x10000 = 65536 takes 3 bytes
  - for a sum type constructor, 1 byte (assumign <=256 total constructors)



  - if we assume that a block size is less than 65536 then any list
    stored in a block will require 3 bytes or less

 - for a node, we need:
   - 1 byte;  tag for N
   - 0 ; tag for pair
   - 3 bytes; tag for ks, which is just length of ks (assume <65536)
   - 3 bytes; tag for rs (assume...)
   - |ks| * k_size ; for ks
   - |rs| * r_size ; for rs; safe to assume r_size is 9 bytes
   - total: 7 + |ks|*(k_size+r_size) + r_size


  - for a leaf, we need:
    - 1 byte; tag for L
    - 3 bytes; tag for kvs list, which is length
    - |kvs| * kv_size; for kvs
    - total: 4 + |kvs|*kv_size

*)
  let ps ~cmp ~k_size ~v_size ~read_k ~write_k ~read_v ~write_v = 
    let r_size = 9 in (* page_ref size in bytes; assume int63 *)
    let max_node_keys = (blk_sz - 7 - r_size) / (k_size + r_size) in
    let max_leaf_size = (blk_sz - 4) / (k_size + v_size) in
    let constants=Constants.({min_leaf_size=2;max_leaf_size;min_node_keys=2; max_node_keys}) in
    object
      method blk_sz=blk_sz
      method page_to_frame=(pg_to_frm ~read_k ~read_v)
      method frame_to_page=(fun blk_sz -> frm_to_pg ~write_k ~write_v ~blk_sz)
      method cmp=cmp
      method constants=constants
      method dbg_ps=None
    end
  in
  ps


