(** Examples for {!Tjr_btree} *)

(** {2 Introduction} 

There are two main types of example: in-memory and on-disk.
*)

(** {2 In-memory store} *)

module Store_in_mem = Store_in_mem

let mk_in_mem_store_ops = Store_in_mem.mk_store_ops


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


(** {2 On-disk store} *)

module Map_on_fd_util = Map_on_fd_util

include Map_on_fd_util.Root_blk

(** {2 Actual examples} *)

module Examples = Examples
