(** Like make_1, but targeting an object *)

(** Construct an example API, using lwt, bin_prot and bigarray *)

open Tjr_monad.With_lwt
open Intf_

(** NOTE hidden doc: blk_id as int *)

(**/**)
module Blk_id = Blk_id_as_int

type blk_id = Blk_id.blk_id[@@deriving bin_io]

(**/**)

let r_size = 9 (* max size of r=blk_id when marshalled *)

(** Use this to construct constants for S if necessary *)
let make_constants ~k_size ~v_size = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size ~v_size

module type S = sig
  type k[@@deriving bin_io]
  type v[@@deriving bin_io]
  val k_cmp: k -> k -> int
  val cs: constants
end

module type T = sig
  type k
  type v
  type t = lwt
  type buf = ba_buf
  type blk = ba_buf
  type r = blk_id
  type ls
  val empty_leaf_as_blk : unit -> blk
  val make :
    blk_dev_ops:(r, blk, t) Tjr_fs_shared.blk_dev_ops ->
    blk_alloc:(r, t) Tjr_fs_shared.blk_allocator_ops ->
    root_ops:(r, t) Tjr_btree.Btree_intf.btree_root_ops -> 
    (k,v,ls)btree
  val make_as_object: open_fd:open_fd -> rt_blk:rt_blk ->
    (k, v, ls) btree
end 

module Make(S:S) : T with type k=S.k and type v=S.v = struct
  include Make_1.Make(S)
  let make ~blk_dev_ops ~blk_alloc ~root_ops =
    make ~blk_dev_ops ~blk_alloc ~root_ops |> fun (x:(module S)) -> 
    let module X = (val x) in
    let open X in
    let ls_create () = root_ops.with_state (fun ~state ~set_state:_ -> 
        X.map_ops_with_ls.leaf_stream_ops.make_leaf_stream state)
    in
    let ls_step = X.map_ops_with_ls.leaf_stream_ops.ls_step in
    let ls_kvs = X.map_ops_with_ls.leaf_stream_ops.ls_kvs in
    object
      method map_ops = map_ops_with_ls
      method flush_cache = flush_cache
      method ls_create = ls_create
      method ls_step = ls_step
      method ls_kvs = ls_kvs
    end

  let make_as_object ~(open_fd:open_fd) ~(rt_blk:rt_blk) : (k,v,ls)btree = 
    make ~blk_dev_ops:open_fd#blk_dev_ops
      ~blk_alloc:rt_blk#blk_alloc
      ~root_ops:rt_blk#with_bt_rt
  let _ = make_as_object
end


(** {2 Int->int example} *)

(** k,v are both int *)
module S_int_int = struct
  open Bin_prot.Std
  type k = int[@@deriving bin_io]
  type v = int[@@deriving bin_io]
  let k_cmp: k -> k -> int = Stdlib.compare
  let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory *)
  let debug_k_and_v_are_int = true
end

module type INT_INT_EX = T with type k=int and type v=int

module Int_int_ex : INT_INT_EX = Make(S_int_int)
