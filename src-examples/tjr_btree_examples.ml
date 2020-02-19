(** Examples for {!Tjr_btree} *)

(** WARNING! Documentation produced by odoc is often incorrect. For
   example, destructive substitution is not handled properly, see {{:https://github.com/ocaml/odoc/issues/96} this issue}. So please
   take care when interpreting this documentation. As an example, the
   documentation for {!module-type:Examples.INT_INT_EX} is incorrect (k
   and v have actually been replaced by int). *)


(** {2 Actual examples} *)

module Make_example = Make_example

module Examples = Examples

(**/**)

(** {2 Internal} *)

(** {2 In-memory store} *)

module Store_in_mem = Store_in_mem

let mk_in_mem_store_ops = Store_in_mem.mk_store_ops


(** {2 Read cache and write-back cache based on LRU} *)

module Store_read_cache = Store_read_cache
module Store_write_back_cache = Store_write_back_cache


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


module Profilers = Profilers_

(**/**)
