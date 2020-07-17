(** Like Tjr_btree.Make, but using Sh_std_ctxt and binprot marshalling,
   and targetting an object; uncached *)

(** Construct an example API, using lwt, bin_prot and bigarray *)

open Tjr_monad.With_lwt
open Intf_

(** NOTE hidden Sh_std_ctxt module binding and open *)

(**/**)
(* shorter types in doc *)
module Shared_ctxt = Shared_ctxt
open Shared_ctxt
(**/**)

module type S = sig
  type k[@@deriving bin_io]
  type v[@@deriving bin_io]
  val k_cmp: k -> k -> int
  val cs: constants
end

module type T = sig
  type k
  type v
  type ls
  val empty_leaf_as_blk : unit -> blk
  val make_as_object: 
    blk_dev_ops:_ blk_dev_ops ->
    blk_alloc:(r,t)blk_allocator_ops ->
    bt_rt:blk_id ->
    (k, v, ls) uncached_btree
end 

module Make(S:S) : T with type k=S.k and type v=S.v = struct
  include S
  (* we want to use Tjr_btree.Make *)
  module S2 = struct
    include S
    include Shared_ctxt
  end

  module M1 = Tjr_btree.Make_1.Make(S2)
  open M1
  type ls = M1.leaf_stream


  module Dnode_mrshlr = Bin_prot_marshalling.Make(
    struct
      include S2
      include M1
      (* let node_cnvs = node_cnvs *)
      let blk_sz = blk_sz
    end)
  let dnode_mshlr = Dnode_mrshlr.dnode_mshlr

  let empty_leaf_as_blk () = 
    dnode_mshlr.dnode_to_blk (Disk_leaf (node_cnvs.kvs_to_leaf []))

  let make_as_object ~blk_dev_ops ~blk_alloc ~bt_rt : (k,v,ls) uncached_btree =
    let open (struct
      let bt_rt = ref bt_rt

      let root_ops = with_ref bt_rt

      let disk_ops = Disk_ops_type.{dnode_mshlr; blk_dev_ops; blk_alloc}
  
      let map_ops_with_ls = disk_to_map ~disk_ops ~root_ops

      let ls_create () = 
        map_ops_with_ls.leaf_stream_ops.make_leaf_stream !bt_rt          
      let ls_step = map_ops_with_ls.leaf_stream_ops.ls_step 
      let ls_kvs = map_ops_with_ls.leaf_stream_ops.ls_kvs
    end)
    in
    object
      method bt_rt = bt_rt
      method with_bt_rt = root_ops
      method map_ops = map_ops_with_ls
      method ls_create = ls_create
      method ls_step = ls_step
      method ls_kvs = ls_kvs
    end

end


(** {2 Int->int example} *)

(** NOTE hidden doc: S_int_int: k,v are both int *)

(**/**)
module S_int_int = struct
  open Bin_prot.Std
  type k = int[@@deriving bin_io]
  type v = int[@@deriving bin_io]
  let k_cmp: k -> k -> int = Stdlib.compare
  let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory? FIXME should assume r is blk_id_as_int? *)
  let debug_k_and_v_are_int = true
end
(**/**)

module type INT_INT_EX = T with type k=int and type v=int

module Int_int_ex : INT_INT_EX = Make(S_int_int)
