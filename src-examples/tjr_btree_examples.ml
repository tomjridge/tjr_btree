(** Examples for {!Tjr_btree} *)

module Intf = Intf_

(** {2 Introduction} 

There are two main types of example: in-memory and on-disk.
*)

(** {2 In-memory store} *)

module Store_in_mem = Store_in_mem

let mk_in_mem_store_ops = Store_in_mem.mk_store_ops


(** {2 Read cache and write-back cache based on LRU} *)

module Store_cache = Store_cache


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


(** {2 Blk layer} *)

(* module Fstore_layer = Fstore_layer *)

module Blk_layer = Blk_layer

(* include Blk_dev_on_fd_util *)


(** {2 Actual examples} *)

module Examples = Examples


(** {2 Internal} *)

module Profilers = Profilers_
