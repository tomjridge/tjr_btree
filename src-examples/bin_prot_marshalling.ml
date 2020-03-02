(** Marshal frames to blocks using binprot. Currently assumes that blk_ids are ints. *)

(** 

Bin_prot marshalling calculations for a type 
{[    type tree = 
      | N of k list * blk_id list 
      | L of (k*v) list [@@deriving bin_io]
]}


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



open Btree_intf
(* open Bin_prot_intf *)

module Blk_id = Blk_id_as_int

open Bin_prot.Std

let int_size_is_63 = assert (Sys.int_size = 63); true

module Make(X: sig 
    (** NOTE we fix page_ref as int *)
    type blk_id = Blk_id.blk_id[@@deriving bin_io]
    type k[@@deriving bin_io]
    type v[@@deriving bin_io]
    type blk = ba_buf
    val blk_sz : blk_sz
    type node
    type leaf
    val node_cnvs: (k,v,blk_id,node,leaf)node_cnvs
  end) 
=
struct
  open X

  type r = blk_id[@@deriving bin_io]

  module BP = Bin_prot

  (** We convert via this datatype, so we can use (at)(at)deriving ;
      FIXME inefficient *)
  module Tree = struct
    type tree = 
      | Impossible (** this is to avoid 0 as a marshalled constructor, which hopefully identifies bugs in marshalling early *)
      | N of k list * blk_id list 
      | L of (k*v) list [@@deriving bin_io]
  end  
  open Tree

  type buf = BP.Common.buf


  let dnode_mshlr = 
    let { node_to_krs; krs_to_node; leaf_to_kvs; kvs_to_leaf } = node_cnvs in
    let open Isa_btree_intf in  (* for node_ops fields *)
    let dn2tree = function
      | Disk_node n -> 
        n |> node_to_krs |> fun (ks,rs) -> 
        Test.assert_(fun () ->
          let b = 1+List.length ks = List.length rs in
          let _ : unit = if not b then Printf.printf "%s: ks rs lengths not aligned, %d %d\n%!" __LOC__ (List.length ks) (List.length rs) in
          assert(b));
        N (ks,rs)
      | Disk_leaf l -> l |> leaf_to_kvs |> fun kvs -> L kvs
    in
    let tree2dn = function
      | Impossible -> failwith ("impossible: "^__LOC__)
      | N (ks,rs) -> 
        Test.assert_(fun () -> 
            let b = 1+List.length ks = List.length rs in
            let _ : unit = 
              if not b then Printf.printf "%s: ks rs lengths not aligned, %d %d\n%!" __LOC__ (List.length ks) (List.length rs)
            in
            assert(b));
        (ks,rs) |> krs_to_node |> fun n -> Disk_node n
      | L kvs -> kvs |> kvs_to_leaf |> fun l -> Disk_leaf l
    in
    (* pull this out because frame_to_page takes an explicit blk_sz; FIXME
       should it? *)
    let blk_sz' = Blk_sz.to_int blk_sz in
    let dnode_to_blk dn = 
      let buf = BP.Common.create_buf blk_sz' in (* NOTE not necessarily zeroed *)
      let n = bin_write_tree buf ~pos:0 (dn|>dn2tree) in
      assert(n<=blk_sz');
      buf
    in
    let blk_to_dnode blk = 
      let _ : unit = 
        if Debug_.debug_enabled then 
          Printf.printf "blk_to_dnode: %s...\n%!" 
            (blk |> Bigstring.to_string |> fun s -> String.sub s 0 4 |> String.escaped)
      in
      let t = bin_read_tree blk ~pos_ref:(ref 0) in
      tree2dn t
    in
    ({ dnode_to_blk; blk_to_dnode; blk_sz }:('a,blk)dnode_mshlr)
end


(** Given the relevant sizes (blocks,keys,values), construct the B-tree size constants *)
let make_constants ~blk_sz ~k_size ~v_size = 
  let blk_sz = blk_sz |> Blk_sz.to_int in
  let r_size = 9 in (* page_ref size in bytes; assume int63 *)
  let max_node_keys = (blk_sz - 7 - r_size) / (k_size + r_size) in
  let max_leaf_size = (blk_sz - 4) / (k_size + v_size) in
  let constants=Isa_btree.Constants.(
      {min_leaf_size=2;max_leaf_size;min_node_keys=2; max_node_keys}) in
  constants

