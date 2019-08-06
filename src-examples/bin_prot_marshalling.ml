(** Marshal frames to blocks using binprot. Currently assumes that blk_ids are ints. *)

open Btree_intf
module Blk_id = Blk_id_as_int


(** {2 A type for holding information about a particular type we marshal (such as k)} *)

type 'k bin_prot_info = {
  max_size: int; (* max number of bytes to marshal a 'k *)
  bin_reader: 'k Bin_prot.Type_class.reader;
  bin_writer: 'k Bin_prot.Type_class.writer
}

open Bin_prot.Std

let int_bin_prot_info = {
  max_size= Bin_prot.Size.Maximum.bin_size_int;
  bin_reader = bin_reader_int;
  bin_writer = bin_writer_int
}

let ss_bin_prot_info = Tjr_fs_shared.Small_string.{
  max_size=3+max_length;
  bin_reader = bin_reader_ss;
  bin_writer = bin_writer_ss
}



(** {2 A type for holding key/value reader writers} *)

type ('k,'v) reader_writers = {
  read_k: 'k Bin_prot.Type_class.reader;
  write_k: 'k Bin_prot.Type_class.writer;
  read_v: 'v Bin_prot.Type_class.reader;
  write_v: 'v Bin_prot.Type_class.writer;
}

(** Various reader/writers *)
module Common_reader_writers = struct
  let int_int = {
    read_k  =bin_reader_int;
    write_k =bin_writer_int;
    read_v  =bin_reader_int;
    write_v =bin_writer_int;
  }

  open Small_string
  let ss_ss = {
    read_k  =bin_reader_ss;
    write_k =bin_writer_ss;
    read_v  =bin_reader_ss;
    write_v =bin_writer_ss;
  }

  let ss_int = {
    read_k  =bin_reader_ss;
    write_k =bin_writer_ss;
    read_v  =bin_reader_int;
    write_v =bin_writer_int;
  }
end



module Internal = struct

  module BP = Bin_prot

  (** NOTE we fix page_ref as int *)
  type page_ref = Blk_id.blk_id [@@deriving bin_io]

  (** We convert via this datatype, so we can use (at)(at)deriving ; FIXME inefficient *)
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

  let make_binprot_marshalling ~(block_ops:'blk blk_ops) ~node_leaf_list_conversions = 
    let Node_leaf_list_conversions.{ node_to_krs; krs_to_node; leaf_to_kvs; kvs_to_leaf } = node_leaf_list_conversions in
    let open Isa_btree_intf in  (* for node_ops fields *)
    let blk_sz = block_ops.blk_sz |> Blk_sz.to_int in
    let dn2bp = function
      | Disk_node n -> n |> node_to_krs |> fun (ks,rs) -> N (ks,rs)
      | Disk_leaf l -> l |> leaf_to_kvs |> fun kvs -> L kvs
    in
    let bp2dn = function
      | N (ks,rs) -> (ks,rs) |> krs_to_node |> fun n -> Disk_node n
      | L kvs -> kvs |> kvs_to_leaf |> fun l -> Disk_leaf l
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
    fun { read_k; write_k; read_v; write_v } ->
      let mp = Btree_intf.{ 
          dnode_to_blk=(dnode_to_blk ~write_k ~write_v (* ~blk_sz *));
          blk_to_dnode=(blk_to_dnode ~read_k ~read_v);
          marshal_blk_size=blk_sz }
      in
      mp

  let _ : 
block_ops:'blk blk_ops ->
node_leaf_list_conversions:('a, 'b, page_ref, 'c, 'd)
                           Node_leaf_list_conversions.node_leaf_list_conversions ->
('a, 'b) reader_writers -> (('c, 'd) dnode, 'blk) marshalling_ops
= make_binprot_marshalling


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
    let blk_sz = blk_sz |> Blk_sz.to_int in
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
    ~(block_ops:'blk blk_ops) 
    ~(node_leaf_list_conversions:('k,'v,Blk_id.blk_id,'node,'leaf)Node_leaf_list_conversions.node_leaf_list_conversions)
    ~reader_writers
  = 
  Internal.make_binprot_marshalling ~block_ops ~node_leaf_list_conversions reader_writers


let _ = make_binprot_marshalling

