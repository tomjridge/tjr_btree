(** Like examples/bin_prot_marshalling, but using generic mshlr types;
   inefficient since we can't take advantage of binprot at the level
   of lists of x *)

(** A problem with this generic marshalling is that we can't take
   advantage of binprot's efficiencies *)

open Btree_intf

module type S = sig
  type k
  type v
  type r
  type blk_id = r
  type blk
  type buf = blk
  type node
  type leaf
  val blk_sz : blk_sz
  val node_cnvs: (k,v,blk_id,node,leaf)node_cnvs
end 

module type T = sig
  type node 
  type leaf
  type blk
  val dnode_mshlr: ((node,leaf)dnode,blk)dnode_mshlr
end

(*
module Make(S:S) = struct
  open S
  (** We convert via this datatype, so we can use (at)(at)deriving ;
      FIXME inefficient *)
  module Tree = struct
    type tree = 
      | Impossible (** this is to avoid 0 as a marshalled constructor, which hopefully identifies bugs in marshalling early *)
      | N of k list * blk_id list 
      | L of (k*v) list
  end  
  open Tree

  let mark' f = f ()

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
      mark' (fun () -> 
          let buf = BP.Common.create_buf blk_sz' in (* NOTE not necessarily zeroed *)
          let n = bin_write_tree buf ~pos:0 (dn|>dn2tree) in
      assert(n<=blk_sz');
      buf)
    in
    let blk_to_dnode blk = 
      let _ : unit = 
        if Debug_.debug_enabled then 
          Printf.printf "blk_to_dnode: %s...\n%!" 
            (blk |> Bigstring.to_string |> fun s -> String.sub s 0 4 |> String.escaped)
      in
      mark' blk2d (fun () ->
      let t = bin_read_tree blk ~pos_ref:(ref 0) in
      tree2dn t)
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


*)
