(** Marshal frames to blocks using binprot *)

module Internal = struct

  open Tjr_fs_shared.Block_ops_type
  open Frame
  open Page_ref_int

  module BP = Bin_prot

  open BP.Std

  type ('k,'v) binprot_tree  = 
      N of 'k list * page_ref list 
    | L of ('k*'v) list [@@deriving bin_io]

  (* open Bigarray *)
  type buf = BP.Common.buf

  let blit_buf_to_bytes
      ?(src_pos=0) ~(src:buf) ?(dst_pos=0) ~(dst:bytes) ~len () 
    = BP.Common.blit_buf_bytes ~src_pos src ~dst_pos dst ~len  

  let blit_bytes_to_buf 
      ?(src_pos=0) ~(src:bytes) ?(dst_pos=0) ~(dst:buf) ~len () 
    = BP.Common.blit_bytes_buf ~src_pos src ~dst_pos dst ~len



  let make_binprot_marshalling ~block_ops = 
    let blk_sz = block_ops.blk_sz in
    let f2bp frm = (
      match frm with
      | Disk_node (ks,rs) -> N (ks,rs)
      | Disk_leaf kvs -> L kvs)
    in
    let bp2f iis = (
      match iis with
      | N (ks,rs) -> Disk_node(ks,rs)
      | L kvs -> Disk_leaf kvs)
    in
    (* pull this out because frame_to_page takes an explicit blk_sz; FIXME
       should it? *)
    let frm_to_pg ~write_k ~write_v (* ~blk_sz *) = 
      let bin_writer' = (bin_writer_binprot_tree write_k write_v) in
      fun (frm:('k,'v)frame) -> 
        let buf = BP.Common.create_buf blk_sz in
        let pos' = frm |> f2bp |> bin_writer'.write buf ~pos:0 in
        let s = Bytes.create blk_sz in
        let () = blit_buf_to_bytes ~src:buf ~dst:s ~len:pos' () in
        s |> block_ops.of_bytes
    in
    let pg_to_frm ~read_k ~read_v = 
      let bin_reader' = bin_reader_binprot_tree read_k read_v in
      fun pg -> 
        let s = pg |> block_ops.to_bytes in
        let buf = BP.Common.create_buf blk_sz in
        let () = blit_bytes_to_buf ~src:s ~dst:buf ~len:blk_sz () in
        let bp = bin_reader'.read buf ~pos_ref:(ref 0) in
        bp|>bp2f
    in
    let _ = assert (Sys.int_size = 63) in (* ensure we are on 64 bit system *)
    fun ~read_k ~write_k ~read_v ~write_v ->
      let mp = Marshalling_ops_type.{ 
          frame_to_page=(frm_to_pg ~write_k ~write_v (* ~blk_sz *));
          page_to_frame=(pg_to_frm ~read_k ~read_v);
          page_size=blk_sz }
      in
      mp

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
  let make_constants ~blk_sz ~k_size ~v_size = 
    let r_size = 9 in (* page_ref size in bytes; assume int63 *)
    let max_node_keys = (blk_sz - 7 - r_size) / (k_size + r_size) in
    let max_leaf_size = (blk_sz - 4) / (k_size + v_size) in
    let constants=Isa_btree.Constants.(
        {min_leaf_size=2;max_leaf_size;min_node_keys=2; max_node_keys}) in
    constants

end

let make_constants = Internal.make_constants
let make_binprot_marshalling = Internal.make_binprot_marshalling
