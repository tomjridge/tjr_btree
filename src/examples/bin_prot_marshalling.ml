(** Marshal frames to blocks using binprot *)

open Tjr_fs_shared.Block_ops_type
open Isa_btree
open Isa_export
open Disk_node
open Isa_export_wrapper
open Tjr_btree

(** {2 Binprot util: max sizes etc} *)


(* int -------------------------------------------------------------- *)

open Bin_prot.Std

let bp_size_int = Bin_prot.Size.Maximum.bin_size_int

let bin_reader_int = bin_reader_int

let bin_writer_int = bin_writer_int


(* small string ----------------------------------------------------- *)

open Tjr_fs_shared.Small_string

let bp_size_ss = 3+max_length

let bin_reader_ss = bin_reader_ss

let bin_writer_ss = bin_writer_ss


module Internal = struct

  module BP = Bin_prot

  open BP.Std
         
  type page_ref = int [@@deriving bin_io]

  type ('k,'v) binprot_tree  = 
      N of 'k list * page_ref list 
    | L of ('k*'v) list [@@deriving bin_io]
  
  type buf = BP.Common.buf

  let blit_buf_to_bytes
      ?(src_pos=0) ~(src:buf) ?(dst_pos=0) ~(dst:bytes) ~len () 
    = BP.Common.blit_buf_bytes ~src_pos src ~dst_pos dst ~len  

  let blit_bytes_to_buf 
      ?(src_pos=0) ~(src:bytes) ?(dst_pos=0) ~(dst:buf) ~len () 
    = BP.Common.blit_bytes_buf ~src_pos src ~dst_pos dst ~len



  let make_binprot_marshalling ~(block_ops:'blk block_ops) ~isa_btree_ops = 
    let node_ops = node_ops isa_btree_ops in
    let leaf_ops = leaf_ops isa_btree_ops in
    let node_of_krs = node_of_krs isa_btree_ops in
    let leaf_of_kvs = leaf_of_kvs isa_btree_ops in
    let blk_sz = block_ops.blk_sz in
    let dn2bp = function
      | Disk_node n -> n |> node_ops.dbg_node_krs |> fun (ks,rs) -> N (ks,rs)
      | Disk_leaf l -> l |> leaf_ops.dbg_leaf_kvs |> fun kvs -> L kvs
    in
    let bp2dn = function
      | N (ks,rs) -> (ks,rs) |> node_of_krs |> fun n -> Disk_node n
      | L kvs -> kvs |> leaf_of_kvs |> fun l -> Disk_leaf l
    in
    (* pull this out because frame_to_page takes an explicit blk_sz; FIXME
       should it? *)
    let dnode_to_blk ~write_k ~write_v (* ~blk_sz *) = 
      let bin_writer' = (bin_writer_binprot_tree write_k write_v) in
      fun dn -> 
        let buf = BP.Common.create_buf blk_sz in
        let pos' = dn |> dn2bp |> bin_writer'.write buf ~pos:0 in
        let s = Bytes.create blk_sz in
        let () = blit_buf_to_bytes ~src:buf ~dst:s ~len:pos' () in
        s |> block_ops.of_bytes
    in
    let blk_to_dnode ~read_k ~read_v = 
      let bin_reader' = bin_reader_binprot_tree read_k read_v in
      fun blk -> 
        let s = blk |> block_ops.to_bytes in
        let buf = BP.Common.create_buf blk_sz in
        let () = blit_bytes_to_buf ~src:s ~dst:buf ~len:blk_sz () in
        let bp = bin_reader'.read buf ~pos_ref:(ref 0) in
        bp|>bp2dn
    in
    let _ = assert (Sys.int_size = 63) in (* ensure we are on 64 bit system *)
    fun ~read_k ~write_k ~read_v ~write_v ->
      let mp = { 
          dnode_to_blk=(dnode_to_blk ~write_k ~write_v (* ~blk_sz *));
          blk_to_dnode=(blk_to_dnode ~read_k ~read_v);
          marshal_blk_size=blk_sz }
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

(** {2 Main bin_prot marshalling functions} *)

(** Given the relevant sizes (blocks,keys,values), construct the B-tree size constants *)
let make_constants = Internal.make_constants

(** Construct the bin_prot marshalling parameters, based on individual
   functions to read and write keys and values. *)
let make_binprot_marshalling 
    ~(block_ops:'blk block_ops) 
    ~(isa_btree_ops: ('k,'v,'r,'t)isa_btree_ops) 
  = 
  Internal.make_binprot_marshalling ~block_ops ~isa_btree_ops



