(** Examples for {!Tjr_btree} *)

(** {2 Introduction} 

There are two main types of example: in-memory and on-disk.
*)

(** {2 In-memory store} *)

include Store_in_mem

let mk_in_mem_store_ops = mk_store_ops


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


(** {2 On-disk store} *)

module Map_on_fd_util = Map_on_fd_util

(** {2 Actual examples} *)

module Examples = Examples
