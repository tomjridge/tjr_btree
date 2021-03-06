(** Examples for {!Tjr_btree} *)

(** Usage: open Tjr_btree;; open Tjr_btree.Btree_intf;; open Tjr_btree_examples *)

(** WARNING! Documentation produced by odoc is often incorrect. For
   example, destructive substitution is not handled properly, see
   {{:https://github.com/ocaml/odoc/issues/96} this issue}. So please
   take care when interpreting this documentation. *)

module Intf_ = Intf_
include Intf_

(** {2 Refined construction} *)

module Make_1 = Make_1


module Make_2 = Make_2


module Make_3 = Make_3


module Open_fd_and_rt_blk = Open_fd_and_rt_blk


(*
(** {2 Root block, for lwt} *)
module Rt_blk = Rt_blk
*)

(** {2 Actual examples} *)

module Int_int_ex = Make_1.Int_int_ex

module Generic_main_v2 = Generic_main_v2
(* module Generic_example = Generic_example *)
module Generic_example_v2 = Generic_example_v2

(**/**)

(** {2 Internal} *)

(** {2 In-memory store} *)

module Store_in_mem = Store_in_mem

let mk_in_mem_store_ops = Store_in_mem.mk_store_ops


(** {2 Read cache and write-back cache based on LRU} *)

(* module Store_read_cache = Store_read_cache *)
module Store_write_back_cache = Tjr_btree.Pvt.Store_write_back_cache


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


module Profilers = Profilers_

(**/**)
