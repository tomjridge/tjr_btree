(** Various examples *)
(* open Tjr_profile.Util.Profiler *)
open Tjr_btree
open Btree_intf
open Btree_examples_intf
open Fstore_layer

(** The steps to construct an example are:

- fix the monad type (eg store passing)
- construct block ops {!Tjr_fs_shared.Block_ops}, which converts
  strings/bytes to/from blks
- construct block device ops {!Tjr_fs_shared.Blk_dev_ops_type}, which
  reads and writes to blks
- implement {!Bin_prot_marshalling.node_leaf_conversions} to convert
  from list-based leaf/node impls to the efficient leaf/node impls
- implement marshalling procedures via {!Bin_prot_marshalling}
- calculate constants based on blk_sz and key and value types, and
  marshalling strategy
- implement blk_allocator_ops, to allow allocation of blks via id
- for every desired combination of (key/value types, marshalling,
  blk_dev, blk_allocator), use {!Tjr_btree.disk_to_store} to
  construct a corresponding store
- then use {!Tjr_btree.store_to_map} to convert store to a map
  (using a root pointer to convert the pre_map_ops to a map_ops)

FIXME include this documentation in main tjr_btree lib, perhaps as a
simple int->int example

{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vQXKtsYnp_Z4gUHTpYZOeLrGGIIQxPQrSSgdnoUylAW269ckYBMaUXz9MlDk8aHd1evYCSJNFGpqRFb/pub?w=960&amp;h=720">
%}

*)




module Internal_print_constants = struct
  let print_constants cs = 
    if !Global.debug_global then 
      Printf.printf "Calculated constants: lmin,%d lmax,%d nmin,%d nmax,%d\n%!" 
        cs.min_leaf_size cs.max_leaf_size cs.min_node_keys cs.max_node_keys
end
open Internal_print_constants


(** {2 Abstract version} *)

module type S = sig
  type k  (* we assume k_cmp = Pervasives.compare *)
  type v
  type r = Blk_id.blk_id
  type t = fstore_passing

  val monad_ops: t monad_ops

  val k_size: int
  val v_size: int 

  val reader_writers: (k,v)Bin_prot_marshalling.reader_writers
end

(* FIXME the result sig of following could well be from isa_btree *)

module Internal(S:S) = struct
  open S
      
  let k_cmp : k -> k -> int = Pervasives.compare

  let constants = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size 

  module S' = struct
    include S
    let k_cmp = k_cmp
    let cs = constants
  end
  module Tjr_btree' = Tjr_btree.Make(S')
  (* let node_ops,leaf_ops = Tjr_btree'.(node_ops,leaf_ops) *)
  include Tjr_btree'

  let node_leaf_list_conversions = Node_leaf_list_conversions.{
      node_to_krs=node_ops.node_to_krs;
      krs_to_node=node_ops.krs_to_node;
      leaf_to_kvs=leaf_ops.leaf_to_kvs;
      kvs_to_leaf=leaf_ops.kvs_to_leaf
    }


  open Blk_layer
  let disk_ops = Blk_layer.make_disk_ops ~blk_dev_ops:on_disk_blk_dev ~reader_writers ~node_leaf_list_conversions
  let _ = disk_ops

  (* NOTE we have to shield so that evaluation is delayed till post-init *)
  let empty_leaf_as_blk () = empty_leaf_as_blk ~disk_ops

  let store_ops = disk_to_store ~disk_ops
  let store_ops = Store_with_lru.make_store_with_lru ~monad_ops ~store_ops

  let pre_btree_ops = store_to_pre_btree ~store_ops

  let root_ops = Fstore_layer.Fstore.root_ops

  let map_ops_etc = pre_btree_to_map ~pre_btree_ops ~root_ops

  (* NOTE we have to shield so that evaluation is delayed till post-init *)
  let btree_from_file = 
    { btree_from_file=(fun ~fn ~create ~init ->
          make_btree_from_file ~empty_leaf_as_blk:(empty_leaf_as_blk ()) |> fun x -> 
        x.btree_from_file ~fn ~create ~init )}


  let _ = btree_from_file
end



(** {2 Instantiations} *)

module Int_int = struct
  module S = struct
    open Bin_prot_marshalling
    type k = int
    type v = int
    type r = Blk_id.blk_id
    type t = fstore_passing

    let monad_ops = Monad_ops.monad_ops

    let k_size = int_bin_prot_info.max_size
    let v_size = int_bin_prot_info.max_size

    let reader_writers = Common_reader_writers.int_int
  end
  include S
  module Internal = Internal(S)
  include Internal
  let _ = print_constants constants
end


module Ss_ss = struct
  module S = struct
    open Bin_prot_marshalling
    type k = ss
    type v = ss
    type r = Blk_id.blk_id
    type t = fstore_passing

    let monad_ops = Monad_ops.monad_ops

    let k_size = ss_bin_prot_info.max_size
    let v_size = ss_bin_prot_info.max_size

    let reader_writers = Common_reader_writers.ss_ss
  end
  include S
  module Internal = Internal(S)
  include Internal
  let _ = print_constants constants
end


module Ss_int = struct
  module S = struct
    open Bin_prot_marshalling
    type k = ss
    type v = int
    type r = Blk_id.blk_id
    type t = fstore_passing

    let monad_ops = Monad_ops.monad_ops

    let k_size = ss_bin_prot_info.max_size
    let v_size = int_bin_prot_info.max_size

    let reader_writers = Common_reader_writers.ss_int
  end
  include S
  module Internal = Internal(S)
  include Internal
  let _ = print_constants constants
end

