(** Examples for {!Tjr_btree} *)

(* module Intf = Intf_ *)
(* module Bin_prot_intf = Bin_prot_intf *)

(** {2 Introduction} 

There are two main types of example: in-memory and on-disk.
*)

(** {2 In-memory store} *)

module Store_in_mem = Store_in_mem

let mk_in_mem_store_ops = Store_in_mem.mk_store_ops


(** {2 Read cache and write-back cache based on LRU} *)

(* module Store_cache = Store_cache *)


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


(** {2 Actual examples} *)

module Examples = Examples


(** {2 Internal} *)

module Profilers = Profilers_



(** {2 Blk layer impl} *)

(* module File_impl = File_impl *)

(* module Fstore_layer = Fstore_layer *)

(* module Blk_layer_2 = Blk_layer_2 *)

(* include Blk_dev_on_fd_util *)

